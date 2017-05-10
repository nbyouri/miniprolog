(ns miniprolog.core-test
  (:require [clojure.test :refer :all]
            [miniprolog.core :refer :all]))

(deftest vartest
  (testing "Couldn't find out whether symbol is a prolog variable or an atom"
    (is (= true (isvar "Type")))))

(deftest )
