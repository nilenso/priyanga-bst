;; Build a binary search tree
;; The tree should support the following operations:
;; - Inserting an integer into the tree
;; - Finding if a given integer is present in the tree
;; - Deleting an integer from the tree
;; The code must have the following characteristics
;; - The tree must be immutable
;; - The tree must be thread safe (two threads can modify the tree simultaneously)
;; - All functions in the code must have a single exit point
;; - Pay attention to memory usage
;; Extra credit:
;; - The tree must be self balancing
;; - Use your tree to find the count of the words in a huge text file

(ns bst.core
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]))

(defn height
  "Returns height of the given tree"
  [tree]
  (cond
    (nil? tree) -1
    (empty? tree) -1
    :else
    (:height tree)))

(defn factor
  "Returns the balance factor of root node of the given tree"
  [{:keys [left right] :as tree}]
  (when (not-empty tree)
    (- (height left) (height right))))

(defn is-left-case?
  "Returns true if left subtree is imbalanced else false"
  [tree]
  (if (empty? tree) false
      (> (factor tree) 1)))

(defn is-right-case?
  "Returns true if right sub tree is imbalanced else false"
  [tree]
  (if (empty? tree) false
      (< (factor tree) -1)))

(defn is-left-right-case?
  "Returns true if right sub tree of left child is imbalanced else false"
  [tree]
  (if (empty? tree) false
      (and (is-left-case? tree) (< (factor (:left tree)) 0))))

(defn is-left-left-case?
  "Returns true if left sub tree of left child is imbalanced else false"
  [tree]
  (if (empty? tree) false
      (and (is-left-case? tree) (> (factor (:left tree)) 0))))

(defn is-right-right-case?
  "Returns true if right sub tree of right child is imbalanced else false"
  [tree]
  (if (empty? tree) false
      (and (is-right-case? tree) (< (factor (:right tree)) 0))))

(defn is-right-left-case?
  "Returns true if right sub tree of left child is imbalanced else false"
  [tree]
  (if (empty? tree) false
      (and (is-right-case? tree) (> (factor (:right tree)) 0))))

(def update-height #(assoc %1 :height (+ 1 (max (height (:left %1)) (height (:right %1))))))

(defn rotate-left
  "Returns the left rotated tree "
  [{:keys [root left right] :as tree}]
  (let [pivot (:root right)
        left-pivot (:left right)
        right-pivot (:right right)]
    (if (empty? tree) {}
        (update-height {:root pivot
                        :left {:root root
                               :left left
                               :right left-pivot
                               :height (+ 1 (max (height left) (height left-pivot)))}
                        :right right-pivot}))))

(defn rotate-right
  "Returns the right rotated tree"
  [{:keys [root left right] :as tree}]
  (let [pivot (:root left)
        left-pivot (:left left)
        right-pivot (:right left)]
    (if (empty? tree) {}
        (update-height
         {:root pivot
          :left left-pivot
          :right {:root root
                  :left right-pivot
                  :right right
                  :height (+ 1 (max (height right-pivot) (height right)))}}))))

(defn balance-subtree
  "Returns a balanced bst"
  [{:keys [left right] :as tree}]
  (cond
    (is-right-left-case? tree) (rotate-left
                                (assoc tree :right (rotate-right right)))

    (is-left-right-case? tree) (rotate-right
                                (assoc tree :left (rotate-left left)))

    (is-right-right-case? tree) (rotate-left tree)

    (is-left-left-case? tree) (rotate-right tree)

    :else tree))

(defn insert-and-balance
  "Returns a bst after inserting a new node"
  [{:keys [root left right] :as tree} value]
  (cond
    (nil? root) {:root value :left nil :right nil :height 0}
    (neg? (compare value root)) (balance-subtree
                                 (update-height
                                  (assoc tree :left (insert-and-balance left value))))
    (pos? (compare value root))  (balance-subtree
                                  (update-height
                                   (assoc tree :right (insert-and-balance right value))))
    :else tree))

(defn create
  "Returns a bst when given a list of node values"
  [values]
  (reduce insert-and-balance {} values))

(defn has?
  "Returns true if if a given integer is present in the tree else false"
  [{:keys [root left right]} value]
  (cond
    (nil? root) false
    (= value root) true
    (neg? (compare value root)) (has? left value)
    :else (has? right value)))

(defn min-node
  "Find the minimun value in a given tree"
  [{:keys [root left] :as tree}]
  (cond
    (nil? tree) nil
    (nil? left) root
    :else (min-node left)))

(defn remove-node
  "Returns a tree after removing the given node from the given tree"
  [{:keys [root left right] :as tree} value]
  (cond
    (empty? tree) {}
    (nil? tree) nil
    (neg? (compare value root)) (balance-subtree
                                 (update tree :left remove-node value))
    (pos? (compare value root)) (balance-subtree
                                 (update tree :right remove-node value))
    (nil? left) right
    (nil? right) left
    :else (let [min (min-node right)]
            (-> (balance-subtree
                 (update tree :right remove-node min))
                (assoc :root min)))))

(defn read-file
  "Returns a vector containing all the words in a file"
  [file-path]
  (let [content (string/trim
                 (slurp file-path))]
    (if (empty? content)
      []
      (string/split content #"[\s+ \. \n]"))))

(defn count-nodes
  "Returns the number of nodes in a tree"
  [{:keys [left right] :as tree}]
  (if (empty? tree)
    0
    (+ 1 (count-nodes left) (count-nodes right))))

(defn count-words
  "Returns the number of unique words in a file"
  [file-path]
  (count-nodes (create (read-file file-path))))
