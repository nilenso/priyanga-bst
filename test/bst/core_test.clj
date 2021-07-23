(ns bst.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [bst.core :refer [create-bst height factor]]))

(deftest create-bst-test
  (let [expected-tree {:root 2
                       :left {:root 1, :left nil, :right nil}
                       :right
                       {:root 7
                        :left {:root 5, :left nil, :right nil}
                        :right {:root 9, :left nil, :right nil}}}] 
  (testing "create-bst failed"
    (is (= expected-tree (create-bst [2 1 9 5 7]))))))

(deftest height-test
  (let [tree {:root 2
                       :left {:root 1, :left nil, :right nil}
                       :right
                       {:root 7
                        :left {:root 5, :left nil, :right nil}
                        :right {:root 9, :left nil, :right nil}}}
        expected-height 3]
    (testing "create-bst failed"
      (is (= expected-height (height tree))))))

(deftest factor-test
  (let [tree {:root 2
              :left {:root 1, :left nil, :right nil}
              :right
              {:root 7
               :left {:root 5, :left nil, :right nil}
               :right {:root 9, :left nil, :right nil}}}
        expected-factor -1]
    (testing "create-bst failed"
      (is (= expected-factor (factor tree))))))
