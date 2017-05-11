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
(defn issym [x]
  (not (isvar x)))

;; get facts/rules from the database
(defn getdb [data]
  (dosync (get @*db* data)))

;; get rule name (rulename :- rulebody)
(defn rulename [rule]
  (first rule))

;; get rule body
(defn rulebody [rule]
  (rest rule))

;; define new rules and facts (& is for varargs)
(defmacro <- [& clause]
  `(addtodb '~clause))

(defmacro ?- [rule]
  (println "RULE --> " rule)
  (let [rules (findrules [] (->Goal rule [rule] {} nil) rule {})]
    (println "QUERY --> " rules)
    `(interpret #{} '~rules)))

;; rule to prolog predicate name and arity (father X Y) -> [father 2]
(defn rulekey [rule]
  (if (seq? (first rule))
    [(ffirst rule) (dec (count (first rule)))]
    [(first rule) (dec (count rule))]))

;; get rules from database
(defn getrules [rules]
  (dosync
    (get @*db* rules)))

;; add rule to the database
(defn addrule [rule]
    (update @*db* (rulekey rule) (fnil conj #{}) (if (seq? (first rule))
                                                   rule
                                                   (list rule))))

(defn addtodb [rule]
  (let [rules (addrule rule)]
    (dosync
      (ref-set *db* rules))))

(defrecord Goal [head terms env parent])

(defn findrules [queue parent term env]
  (reduce
    (fn [acc rule]
      (if-let [ans (unify term env (first rule) {})]
        (conj acc (->Goal (first rule) (next rule) ans parent))
        acc))
    queue
    (*db* (rulekey term))))

(defn interpret [acc queue]
  (println "queue " queue)
  (let [{:keys [head terms env parent] :as goal} (peek queue)]
    (println "goal " (:head goal))
    (if-not goal
      acc
      (let [queue (pop queue)]
        (println "let queue " queue)
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


(defn lvar? [x]
  (symbol? x))

(defn constant? [x]
  (and
       (not (lvar? x))
       (not (seq? x))))

;; Solve a query with it's environment
(defn solve [src src-env]
  (cond
    (constant? src) src
    (lvar? src) (when-let [ans (get src-env src)]
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
                                
                  
(defn unify [src src-env dest dest-env]
  (assert (and (not (nil? src))
               (not (nil? dest))))
  (cond
    (and (constant? src)
         (constant? dest)) (and (= src dest)
                                dest-env)
    
    (lvar? src) (if-let [resolved (solve src src-env)]
                  (recur resolved src-env dest dest-env)
                  dest-env)
    
    (lvar? dest) (if-let [resolved (solve dest dest-env)]
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







