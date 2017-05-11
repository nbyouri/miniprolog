(ns miniprolog.core-test
  (:require [clojure.test :refer :all]
            [miniprolog.core :refer :all]))

(deftest vartest
  (testing "Couldn't find out whether symbol is a prolog variable or an atom"
    (is (= true (isvar "Type")))))

(deftest simplequery
  (testing "simple query")
  (<- (father george maria))
  (<- (father george howard))
  (<- (father george roger))
  (<- (father george laura))
  (<- (father albert tamara))
  (<- (father albert alexandra))
  (<- (father albert jessica))
  (<- (father roger brandon))
  (<- (father roger nadia))
  (<- (father bob frank))
  (<- (father bob anthony))

  (<- (mother cecilia howard))
  (<- (mother cecilia roger))
  (<- (mother cecilia laura))
  (<- (mother maria tamara))
  (<- (mother maria alexandra))
  (<- (mother maria jessica))
  (<- (mother sarah brandon))
  (<- (mother sarah nadia))
  (<- (mother laura frank))
  (<- (mother laura anthony))

  (<- (male george))
  (<- (male albert))
  (<- (male roger))
  (<- (male howard))
  (<- (male bob))
  (<- (male brandon))
  (<- (male frank))
  (<- (male anthony))

  (<- (female cecilia))
  (<- (female maria))
  (<- (female sarah))
  (<- (female laura))
  (<- (female tamara))
  (<- (female alexandra))
  (<- (female jessica))
  (<- (female nadia))
  
  (<- (parent Parent Child) (father Parent Child))
  (<- (parent Parent Child) (mother Parent Child))
  
  (<- (ancestor X Z) (parent X Z))
  (<- (ancestor X Z) (parent X Y) (ancestor Y Z))
  
  (<- (append (list) T T))
  (<- (append (list H T) L2 (list H TR))
      (append T L2 TR))
  
  (is (= (?- (male X))
         '#{{X george}
            {X albert}
            {X roger}
            {X howard}
            {X bob}
            {X brandon}
            {X frank}
            {X anthony}}))
  
  (is (= (?- (father george X))
         '#{{X maria}
            {X howard}
            {X roger}
            {X laura}}))

  (is (= (?- (father X Y))
         '#{{X george Y maria}
            {X george Y howard}
            {X george Y roger}
            {X george Y laura}
            {X albert Y tamara}
            {X albert Y alexandra}
            {X albert Y jessica}
            {X roger Y brandon}
            {X roger Y nadia}
            {X bob Y frank}
            {X bob Y anthony}}))
  
  (is (= (?- (ancestor X tamara))
         '#{{X maria}
            {X albert}
            {X george}}))
  
  (def ans (?- (append (list 1 (list)) (list 2 (list)) L)))
  
  (is (= ans '#{{L (list 1 (list 2 (list)))}}))
  
  (def ans2 (?- (append (list 1 (list 2 (list))) (list 3 (list 4 (list))) R)))

  (is (= ans2 '#{{R (list 1 (list 2 (list 3 (list 4 (list)))))}})))

 


         
  
 
