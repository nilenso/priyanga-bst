(ns bst.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [bst.core :refer [create-bst]]))

(deftest create-bst-test
  (let [expected-tree {:root 2
                       :left {:root 1, :left nil, :right nil}
                       :right
                       {:root 7
                        :left {:root 5, :left nil, :right nil}
                        :right {:root 9, :left nil, :right nil}}}] 
  (testing "create-bst failed"
    (is (= expected-tree (create-bst [2 1 9 5 7]))))))


