(ns miniprolog.core)

;; Facts and rules database
(def ^:dynamic *db* (ref {}))

;; reset the database
(defn reset []
  (dosync (ref-set *db* {})))

;; detect a variable
(defn isvar [x]
  (Character/isUpperCase (first (str x))))

;; detect a symbol
(defn isatom [x]
  (and
       (not (isvar x))
       (not (seq? x))))

;; rule to prolog predicate name and arity (father X Y) -> [father 2]
(defn rulekey [rule]
  (if (seq? (first rule))
    [(ffirst rule) (dec (count (first rule)))]
    [(first rule) (dec (count rule))]))

;; append rules
(defn addrule [rule]
    (update @*db* (rulekey rule) (fnil conj #{}) (if (seq? (first rule))
                                                   rule
                                                   (list rule))))
;; update rule db
(defn addtodb [rule]
  (let [rules (addrule rule)]
    (dosync
      (ref-set *db* rules))))

;; record of goals
(defrecord Goal [head terms env parent])

;; Solve a query with it's environment
(defn solve [src src-env]
  (cond
    (isatom src) src
    (isvar src) (when-let [ans (get src-env src)]
                  (recur ans src-env))
    :else (let [[head & args] src
                _ (assert (symbol? head) "Error")
                rargs (reduce
                        (fn [acc v]
                          (if-let [ans (solve v src-env)]
                            (conj acc ans)
                            (reduced nil)))
                        []
                        args)]
            (when rargs
              (cons head rargs)))))
                                
;; unification of two queries and their environment                  
(defn unify [src src-env dest dest-env]
  (assert (and (not (nil? src))
               (not (nil? dest))))
  (cond
    (and (isatom src)
         (isatom dest)) (and (= src dest)
                             dest-env)
    
    (isvar src) (if-let [resolved (solve src src-env)]
                  (recur resolved src-env dest dest-env)
                  dest-env)
    
    (isvar dest) (if-let [resolved (solve dest dest-env)]
                   (recur src src-env resolved dest-env)
                   (assoc dest-env dest (solve src src-env)))
    
    (not (and (seq? src)
              (seq? dest))) (assert false (str "Bad input" src dest))
    
    (not= (first src) (first dest)) nil
    (not= (count src) (count dest)) nil
    
    :else (loop [dest-env dest-env
                 src-seq (next src)
                 dest-seq (next dest)]
            (if src-seq
              (let [sarg (first src-seq)
                    darg (first dest-seq)]
                (when-let [new-env (unify sarg src-env darg dest-env)]
                  (recur new-env (next src-seq) (next dest-seq))))
              dest-env))))

;; find a rule based on predicate name and arity
(defn findrules [queue parent term env]
  (reduce
    (fn [acc rule]
      (if-let [ans (unify term env (first rule) {})]
        (conj acc (->Goal (first rule) (next rule) ans parent))
        acc))
    queue
    (*db* (rulekey term))))


;; interpret a set of goals
(defn interpret [acc queue]
  ;(println "queue " queue)
  (let [{:keys [head terms env parent] :as goal} (peek queue)]
    ;(println "goal " (:head goal))
    (if-not goal
      acc
      (let [queue (pop queue)]
        ;(println "let queue " queue)
        (if-not terms
          (if-not parent
            (let [acc (conj acc env)]
              (if (reduced? acc)     
                acc
                (recur acc queue)))
            (let [parent-env (unify head env (-> parent :terms first) (:env parent))]
              (recur acc (conj queue (assoc parent :env parent-env
                                       :terms (-> parent :terms next))))))
          (let [term  (first terms)
                queue (findrules queue goal term env)]
            (recur acc queue)))))))




;; define new rules and facts
(defmacro <- [& clause]
  `(addtodb '~clause))

;; human output for the result set
(defn output [result]
  ;; if #{} or #{{}}
  (if (empty? (first result))
    (case (count result)
      0 false
      1 true)
    ;; else return the set
    result))


;; apply a query to the database
(defn query [acc query]
  ;(println "INPUT " query)
  (let [rule (first query)]
    ;(println "RULE --> " rule)
    (if-not rule
      ; Return the first and only element of the list
      (output (first acc))
      (let [rules (findrules [] (->Goal rule [rule] {} nil) rule {})]
        ;(println "QUERY --> " rules)
        (let [result (conj acc (interpret #{} rules))]
          (recur result (rest query)))))))


;; query macro
(defmacro ?- [& query]
  `(query nil '~query))


