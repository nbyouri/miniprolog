(ns miniprolog.core)

;; Facts and rules database
(def ^:dynamic *db* (ref {}))

;; reset the database
(defn reset []
  (dosync (ref-set *db* {})))

;; detect a variable
(defn isvar [x]
  (Character/isUpperCase (first (str x))))

;; detect an atom
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
    ;; if the source is an atom the just return it
    (isatom src) src
    
    ;; if src is a variable then recur with the extracted value
    ;; for the variable src in src-env
    (isvar src) (when-let [ans (get src-env src)]
                  (recur ans src-env))
    
    ;; otherwise solve the src with src-env
    :else (let [[head & args] src
                _ (assert (symbol? head) "Error")
                rargs (reduce
                        (fn [acc v]
                          (if-let [ans (solve v src-env)]
                            (conj acc ans)
                            (reduced nil)))
                        []
                        args)]
            ;; create a cons of head and rargs if reduce properly applied
            (when rargs
              (cons head rargs)))))
                                
;; Unification of two queries and their environment
;; see the comments in the method for an explanation
;; it supports nesting
(defn unify [src src-env dest dest-env]
  ;; make sure src and dest are not nil
  (assert (and (not (nil? src))
               (not (nil? dest))))
  (cond
    ;; if src and dest are atoms and they're equal then simply return the
    ;; dest-env. Example: (unify 'youri {} 'youri {})
    ;; will return {}.
    (and (isatom src)
         (isatom dest)) (and (= src dest)
                             dest-env)
    
    ;; if src is a variable it will run unify recursively
    ;; after solving with the src-env. Exemple: (unify 'Youri {'Youri 23} 1 {})
    ;; will return false because it will run unify again
    ;; with (unify 23 {'Youri 23} 1 {}) which can not be unified.
    ;; If the src and src-env cannot be solved, the dest-env is returned.
    ;; Exemple: (unify 'Youri '{Like apple} 1 '{Like beans}) will return {Like beans}.
    (isvar src) (if-let [resolved (solve src src-env)]
                  (recur resolved src-env dest dest-env)
                  dest-env)
    
    ;; if dest if a variable then dest is solved with it's dest-env. If solving
    ;; succeeds then recur with the newly solved dest.
    (isvar dest) (if-let [resolved (solve dest dest-env)]
                   (recur src src-env resolved dest-env)
                   (assoc dest-env dest (solve src src-env)))
    
    ;; fail if both src and dest aren't seq
    (not (and (seq? src)
              (seq? dest))) (assert false (str "Bad input" src dest))
    
    ;; if we have a predicate in src and dest the following code is hit
    ;; first make sure the predicates have the same name
    (not= (first src) (first dest)) nil
    ;; make sure they have the same arity
    (not= (count src) (count dest)) nil
    
    ;; for example, (unify '(father youri) {} '(father X) {})
    ;; will return {X youri}
    :else (loop [dest-env dest-env
                 ;; step through arguments of src
                 src-seq (next src)
                 ;; and dest
                 dest-seq (next dest)]
            (if src-seq
              ;; unify on both current arguments
              (let [sarg (first src-seq)
                    darg (first dest-seq)]
                (when-let [new-env (unify sarg src-env darg dest-env)]
                  ;; recur with the new dest-env if it unifies
                  (recur new-env (next src-seq) (next dest-seq))))
              dest-env))))

;; find a rule based on predicate name and arity
(defn findrules [queue parent term env]
  ;; Apply to all of the queue of rules
  (reduce
    (fn [acc rule]
      ;; Unify the term with the rules in the db
      (if-let [ans (unify term env (first rule) {})]
        ;; Conjoin the accumulator with the newly found goal
        (conj acc (->Goal (first rule) (next rule) ans parent))
        acc))
    queue
    ;; Ge the matching predicate from the database
    (*db* (rulekey term))))


;; Interpret a set of goals, results of each iteration are conjoined in the accumulator
;; which will contain the final result
(defn interpret [acc queue]
  ;; See if a goal is (see the defrecord) matching in the queue
  (let [{:keys [head terms env parent] :as goal} (peek queue)]
    (if-not goal
      ;; Return the accumulator if no more goals are present
      acc
      ;; Actually get the goal
      (let [queue (pop queue)]
        (if-not terms
          (if-not parent
            (let [acc (conj acc env)]
              (if (reduced? acc)     
                acc
                (recur acc queue)))
            ;; Unify with the previous iteration
            (let [parent-env (unify head env (-> parent :terms first) (:env parent))]
              ;; Loop with the newly unified parent environment to unify on 
              (recur acc (conj queue (assoc parent :env parent-env
                                       :terms (-> parent :terms next))))))
          ;; Get the matching rule in the db and loop
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
  ;; Get the first part of the query
  (let [rule (first query)]
    (if-not rule
      ;; Return the first and only element of the list
      (output (first acc))
      ;; Find predicates matching the query to unify
      (let [rules (findrules [] (->Goal rule [rule] {} nil) rule {})]
        ;; Run the main unification loop
        (let [result (conj acc (interpret #{} rules))]
          ;; Recur on the rest of the query
          (recur result (rest query)))))))


;; query macro
(defmacro ?- [& query]
  `(query nil '~query))
