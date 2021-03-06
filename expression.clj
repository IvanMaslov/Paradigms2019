(definterface Expression (evaluate [vars]) (^String toString []) (diff [str]))
(defn evaluate [exp, vars] (.evaluate (exp) vars))
(defn toString [exp] (.toString (exp)))
(defn diff [exp, str] (.diff (exp) str))

(deftype Const [val]
  Expression
  (evaluate [this vars] val)
  (toString [this] (format "%.1f" val))
  (diff [this str] (fn [] (Const. 0))))
(defn Constant [val] (fn [] (Const. val)))

(deftype Var [val]
  Expression
  (evaluate [this vars] (get vars val))
  (toString [this] val)
  (diff [this str] (if (= str val) (fn [] (Const. 1)) (fn [] (Const. 0)))))
(defn Variable [val] (fn [] (Var. val)))


(deftype AbstractMultiOperation [operator differ stringView args]
  Expression
  (evaluate [this vars] (apply operator (map (fn [i] (evaluate i vars)) args)))
  (toString [this] (str "(" stringView " " (clojure.string/join " " (map (fn [i] (toString i)) args)) ")"))
  (diff [this str] (apply differ str args)))

(defn Add [& args] (fn [] (AbstractMultiOperation. + (fn [str & args] (apply Add (map (fn [i] (diff i str)) args))) "+" args)))
(defn Subtract [& args] (fn [] (AbstractMultiOperation. - (fn [str & args] (apply Subtract (map (fn [i] (diff i str)) args))) "-" args)))

(def get_one_diff_map)

(defn Multiply [& args] (fn [] (AbstractMultiOperation. *
                                                        (fn [str & args] (apply Add (apply get_one_diff_map str args))) "*" args)))

(defn div2 [lhs rhs] (/ lhs (double rhs)))
(defn Divide [& args] (fn [] (AbstractMultiOperation. (fn [& args] (reduce div2 args)) (fn [str & args]
                                                                                         (Divide (apply Subtract (apply get_one_diff_map str args))
                                                                                                 (apply Multiply (next args))
                                                                                                 (apply Multiply (next args)))) "/" args)))


(deftype AbstractUnaryOperation [operator differ stringView arg]
  Expression
  (evaluate [this vars] (operator (evaluate arg vars)))
  (toString [this] (str "(" stringView " " (toString arg) ")"))
  (diff [this str] (differ str arg)))


(defn Negate [arg] (fn [] (AbstractUnaryOperation. (fn [t] (- t))
                                                 (fn [str arg] (Negate (diff arg str)))
                                                 "negate" arg)))

(defn get_one_diff_map [str & args] (for [i (range 0 (count args))]
                                      (apply Multiply
                                             (concat
                                               (take i args)
                                               [(diff (nth args i) str)]
                                               (nthnext args (+ i 1))))))

(def operations {'+ Add '- Subtract '* Multiply '/ Divide 'negate Negate})
(defn operation [arg] (get operations arg))

(defn parse [exp] (cond
                    (number? exp) (Constant exp)
                    (symbol? exp) (Variable (str exp))
                    (seq? exp) (apply (operations (first exp)) (map parse (next exp)))))

(defn parseObject [string] (parse (read-string string)))


(println (evaluate (parseObject "(+ x 2.0)") {"x" 3.0}))
