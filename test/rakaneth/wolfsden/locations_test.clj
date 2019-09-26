(ns rakaneth.wolfsden.locations-test
  (:require [clojure.test :refer :all]
            [rakaneth.wolfsden.locations :refer :all]))

(deftest loc-test
  (testing "Location instantiation"
    (let [l (loc 1 2)
          v (loc [1 2])]
      (is (and (= (.x l) 1)) (= (.y l) 2))
      (is (and (= (.x v) 1)) (= (.y v) 2))))
  (testing "Location destructuring"
    (let [[x y] (loc 1 2)]
      (is (= x 1))
      (is (= y 2))))
  (testing "Location equality"
    (let [a (loc 1 2)
          b (loc 1 2)]
      (is (= a b))))
  (testing "Location translation"
    (let [a (loc 1 2)
          b (loc 3 4)
          c (loc-add a b)
          d (loc-add a 3 4)]
      (is (= c (loc 4 6)))
      (is (= d (loc 4 6)))))
  (testing "Location translation by direction"
    (let [a (loc 1 2)
          na (loc-move-dir a :N)
          nea (loc-move-dir a :NE)
          ea (loc-move-dir a :E)
          sea (loc-move-dir a :SE)
          sa (loc-move-dir a :S)
          swa (loc-move-dir a :SW)
          wa (loc-move-dir a :W)
          nwa (loc-move-dir a :NW)]
      (is (= na (loc 1 1)))
      (is (= nea (loc 2 1)))
      (is (= ea (loc 2 2)))
      (is (= sea (loc 2 3)))
      (is (= sa (loc 1 3)))
      (is (= swa (loc 0 3)))
      (is (= wa (loc 0 2)))
      (is (= nwa (loc 0 1)))))
  (testing "Location identification"
    (is (loc? (loc 1 2)))
    (is (not (loc? [1 2]))))
  (testing "Location distance formulae"
    (let [a (loc 0 0)
          b (loc 3 4)]
      (is (= 4 (c-distance a b)))
      (is (= 5.0 (e-distance a b)))
      (is (= 7 (m-distance a b)))))
  (testing "Location adjacency"
    (let [a (loc 1 2)
          b (loc 3 4)
          c (loc 2 2)]
      (is (adj? a c))
      (is (not (adj? a b))))))
