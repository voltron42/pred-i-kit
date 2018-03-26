(ns pred-i-kit.core-test
  (:require [clojure.test :refer :all]
            [pred-i-kit.core :refer :all]
            [clojure.string :as str]))


(deftest test-matching
  (let [test-data (for [a (range 4)
                        b (range 4)
                        c (range 4)]
                    (str/join (concat (repeat a "a") (repeat b "b") (repeat c "c"))))]

    (is (= (filterv (matches? "abc") test-data) ["abc"]))
    (is (= (filterv (matches? "abcc") test-data) ["abcc"] ))

    (is (= (set (filter (matches? #"ab?c") test-data))  #{"ac" "abc"}))
    (is (= (set (filter (matches? #"ab*c") test-data))  #{"ac" "abc" "abbc" "abbbc"}))
    (is (= (set (filter (matches? #"ab+c") test-data))  #{"abc" "abbc" "abbbc"}))
    (is (= (set (filter (matches? #"a*b?c") test-data))  #{"c" "bc" "ac" "abc" "aac" "aabc" "aaac" "aaabc"}))

    (is (= (set (filter (matches? #{"ac"}) test-data))  #{"ac"}))
    (is (= (set (filter (matches? #{"a" "ab" "abc" "abcd"}) test-data))  #{"a" "ab" "abc"}))
    (is (= (set (filter (matches? #{"a" "ab" "abc" "abcd" "bc" "bcd" "cd"}) test-data))  #{"a" "ab" "abc" "bc"}))

    ))

(deftest test-named-as
  (let [base-list (filter not-empty
                          (for [a (range 4)
                                b (range 4)
                                c (range 4)]
                            (str/join (concat (repeat a "a") (repeat b "b") (repeat c "c")))))
        test-data (flatten
                    (concat
                      (map keyword base-list)
                      (map symbol base-list)
                      (for [my-ns base-list
                            my-name base-list]
                        [(symbol my-ns my-name) (keyword my-ns my-name)])))]

    (is (= (set (filterv (named-as "abc") test-data))
           '#{abc :abc}))
    (is (= (set (filterv (named-as "abcc") test-data))
           '#{abcc :abcc}))

    (is (= (set (filter (named-as #"ab?c") test-data))
           '#{ac :ac abc :abc}))
    (is (= (set (filter (named-as #"ab*c") test-data))
           '#{ac abc abbc abbbc :ac :abc :abbc :abbbc}))
    (is (= (set (filter (named-as #"ab+c") test-data))
           '#{abc abbc abbbc :abc :abbc :abbbc}))
    (is (= (set (filter (named-as #"a*b?c") test-data))
           '#{c bc ac abc aac aabc aaac aaabc :c :bc :ac :abc :aac :aabc :aaac :aaabc}))

    (is (= (set (filter (named-as #{"ac"}) test-data))
           '#{ac :ac}))
    (is (= (set (filter (named-as #{"a" "ab" "abc" "abcd"}) test-data))
           '#{a ab abc :a :ab :abc}))
    (is (= (set (filter (named-as #{"a" "ab" "abc" "abcd" "bc" "bcd" "cd"}) test-data))
           '#{a ab abc bc :a :ab :abc :bc}))

    (is (= (set (filterv (named-as "abc" "abc") test-data))
           '#{abc/abc :abc/abc}))
    (is (= (set (filterv (named-as #"ab?c" #{"ab" "ac" "bc"}) test-data))
           '#{ac/ab ac/ac ac/bc abc/ab abc/ac abc/bc :ac/ab :ac/ac :ac/bc :abc/ab :abc/ac :abc/bc}))

    ))



(deftest test-count-lists
  (let [test-lists (map range (range 2 12))]
    (is (= test-lists '((0 1)
                         (0 1 2)
                         (0 1 2 3)
                         (0 1 2 3 4)
                         (0 1 2 3 4 5)
                         (0 1 2 3 4 5 6)
                         (0 1 2 3 4 5 6 7)
                         (0 1 2 3 4 5 6 7 8)
                         (0 1 2 3 4 5 6 7 8 9)
                         (0 1 2 3 4 5 6 7 8 9 10))))

    (is (= (filter (min-count 3) test-lists) '( (0 1 2)
                                                (0 1 2 3)
                                                (0 1 2 3 4)
                                                (0 1 2 3 4 5)
                                                (0 1 2 3 4 5 6)
                                                (0 1 2 3 4 5 6 7)
                                                (0 1 2 3 4 5 6 7 8)
                                                (0 1 2 3 4 5 6 7 8 9)
                                                (0 1 2 3 4 5 6 7 8 9 10))))
    (is (= (filter (min-count 5) test-lists) '( (0 1 2 3 4)
                                                (0 1 2 3 4 5)
                                                (0 1 2 3 4 5 6)
                                                (0 1 2 3 4 5 6 7)
                                                (0 1 2 3 4 5 6 7 8)
                                                (0 1 2 3 4 5 6 7 8 9)
                                                (0 1 2 3 4 5 6 7 8 9 10))))
    (is (= (filter (min-count 7) test-lists) '( (0 1 2 3 4 5 6)
                                                (0 1 2 3 4 5 6 7)
                                                (0 1 2 3 4 5 6 7 8)
                                                (0 1 2 3 4 5 6 7 8 9)
                                                (0 1 2 3 4 5 6 7 8 9 10))))
    (is (= (filter (min-count 7) test-lists) '( (0 1 2 3 4 5 6)
                                                (0 1 2 3 4 5 6 7)
                                                (0 1 2 3 4 5 6 7 8)
                                                (0 1 2 3 4 5 6 7 8 9)
                                                (0 1 2 3 4 5 6 7 8 9 10))))
    (is (= (filter (min-count 10) test-lists) '((0 1 2 3 4 5 6 7 8 9)
                                                 (0 1 2 3 4 5 6 7 8 9 10))))

    (is (= (filter (max-count 3) test-lists) '( (0 1)
                                                (0 1 2))))
    (is (= (filter (max-count 5) test-lists) '( (0 1)
                                                (0 1 2)
                                                (0 1 2 3)
                                                (0 1 2 3 4))))
    (is (= (filter (max-count 7) test-lists) '( (0 1)
                                                (0 1 2)
                                                (0 1 2 3)
                                                (0 1 2 3 4)
                                                (0 1 2 3 4 5)
                                                (0 1 2 3 4 5 6))))
    (is (= (filter (max-count 10) test-lists) '( (0 1)
                                                 (0 1 2)
                                                 (0 1 2 3)
                                                 (0 1 2 3 4)
                                                 (0 1 2 3 4 5)
                                                 (0 1 2 3 4 5 6)
                                                 (0 1 2 3 4 5 6 7)
                                                 (0 1 2 3 4 5 6 7 8)
                                                 (0 1 2 3 4 5 6 7 8 9))))

    (is (= (filter (count-between 3 10) test-lists) '( (0 1 2)
                                                       (0 1 2 3)
                                                       (0 1 2 3 4)
                                                       (0 1 2 3 4 5)
                                                       (0 1 2 3 4 5 6)
                                                       (0 1 2 3 4 5 6 7)
                                                       (0 1 2 3 4 5 6 7 8)
                                                       (0 1 2 3 4 5 6 7 8 9))))
    (is (= (filter (count-between 5 10) test-lists) '( (0 1 2 3 4)
                                                       (0 1 2 3 4 5)
                                                       (0 1 2 3 4 5 6)
                                                       (0 1 2 3 4 5 6 7)
                                                       (0 1 2 3 4 5 6 7 8)
                                                       (0 1 2 3 4 5 6 7 8 9))))
    (is (= (filter (count-between 7 10) test-lists) '( (0 1 2 3 4 5 6)
                                                       (0 1 2 3 4 5 6 7)
                                                       (0 1 2 3 4 5 6 7 8)
                                                       (0 1 2 3 4 5 6 7 8 9))))
    (is (= (filter (count-between 3 7) test-lists) '( (0 1 2)
                                                      (0 1 2 3)
                                                      (0 1 2 3 4)
                                                      (0 1 2 3 4 5)
                                                      (0 1 2 3 4 5 6))))
    (is (= (filter (count-between 3 5) test-lists) '( (0 1 2)
                                                      (0 1 2 3)
                                                      (0 1 2 3 4))))
    (is (= (filter (count-between 5 7) test-lists) '(  (0 1 2 3 4)
                                                      (0 1 2 3 4 5)
                                                      (0 1 2 3 4 5 6))))

    (is (= (filter (exact-count 3) test-lists) '( (0 1 2))))
    (is (= (filter (exact-count 5) test-lists) '( (0 1 2 3 4))))
    (is (= (filter (exact-count 7) test-lists) '( (0 1 2 3 4 5 6))))
    (is (= (filter (exact-count 10) test-lists) '((0 1 2 3 4 5 6 7 8 9))))

    ))

(deftest test-count-strings
  (let [test-data (set (for [a (range 6) b (range 6)]
                         (str/join (concat (repeat a "a") (repeat b "b")))))]
    (is (= test-data #{"" "b" "bb" "bbb" "bbbb" "bbbbb"
                       "a" "ab" "abb" "abbb" "abbbb" "abbbbb"
                       "aa" "aab" "aabb" "aabbb" "aabbbb" "aabbbbb"
                       "aaa" "aaab" "aaabb" "aaabbb" "aaabbbb" "aaabbbbb"
                       "aaaa" "aaaab" "aaaabb" "aaaabbb" "aaaabbbb" "aaaabbbbb"
                       "aaaaa" "aaaaab" "aaaaabb" "aaaaabbb" "aaaaabbbb" "aaaaabbbbb"}))

    (is (= (set (filter (min-count 3) test-data)) #{"bbb" "bbbb" "bbbbb"
                                                    "abb" "abbb" "abbbb" "abbbbb"
                                                    "aab" "aabb" "aabbb" "aabbbb" "aabbbbb"
                                                    "aaa" "aaab" "aaabb" "aaabbb" "aaabbbb" "aaabbbbb"
                                                    "aaaa" "aaaab" "aaaabb" "aaaabbb" "aaaabbbb" "aaaabbbbb"
                                                    "aaaaa" "aaaaab" "aaaaabb" "aaaaabbb" "aaaaabbbb" "aaaaabbbbb"}))
    (is (= (set (filter (min-count 5) test-data)) #{"bbbbb"
                                                    "abbbb" "abbbbb"
                                                    "aabbb" "aabbbb" "aabbbbb"
                                                    "aaabb" "aaabbb" "aaabbbb" "aaabbbbb"
                                                    "aaaab" "aaaabb" "aaaabbb" "aaaabbbb" "aaaabbbbb"
                                                    "aaaaa" "aaaaab" "aaaaabb" "aaaaabbb" "aaaaabbbb" "aaaaabbbbb"}))
    (is (= (set (filter (min-count 7) test-data)) #{"aabbbbb"
                                                    "aaabbbb" "aaabbbbb"
                                                    "aaaabbb" "aaaabbbb" "aaaabbbbb"
                                                    "aaaaabb" "aaaaabbb" "aaaaabbbb" "aaaaabbbbb"}))
    (is (= (set (filter (min-count 10) test-data)) #{"aaaaabbbbb"}))

    (is (= (set (filter (max-count 3) test-data)) #{"" "b" "bb" "bbb"
                                                    "a" "ab" "abb"
                                                    "aa" "aab"
                                                    "aaa"}))
    (is (= (set (filter (max-count 5) test-data)) #{"" "b" "bb" "bbb" "bbbb" "bbbbb"
                                                    "a" "ab" "abb" "abbb" "abbbb"
                                                    "aa" "aab" "aabb" "aabbb"
                                                    "aaa" "aaab" "aaabb"
                                                    "aaaa" "aaaab"
                                                    "aaaaa"}))
    (is (= (set (filter (max-count 7) test-data)) #{"" "b" "bb" "bbb" "bbbb" "bbbbb"
                                                    "a" "ab" "abb" "abbb" "abbbb" "abbbbb"
                                                    "aa" "aab" "aabb" "aabbb" "aabbbb" "aabbbbb"
                                                    "aaa" "aaab" "aaabb" "aaabbb" "aaabbbb"
                                                    "aaaa" "aaaab" "aaaabb" "aaaabbb"
                                                    "aaaaa" "aaaaab" "aaaaabb"}))
    (is (= (set (filter (max-count 10) test-data)) #{"" "b" "bb" "bbb" "bbbb" "bbbbb"
                                                     "a" "ab" "abb" "abbb" "abbbb" "abbbbb"
                                                     "aa" "aab" "aabb" "aabbb" "aabbbb" "aabbbbb"
                                                     "aaa" "aaab" "aaabb" "aaabbb" "aaabbbb" "aaabbbbb"
                                                     "aaaa" "aaaab" "aaaabb" "aaaabbb" "aaaabbbb" "aaaabbbbb"
                                                     "aaaaa" "aaaaab" "aaaaabb" "aaaaabbb" "aaaaabbbb" "aaaaabbbbb"}))

    (is (= (set (filter (exact-count 3) test-data)) #{"bbb" "abb" "aab" "aaa"}))
    (is (= (set (filter (exact-count 5) test-data)) #{"bbbbb" "abbbb" "aabbb" "aaabb" "aaaab" "aaaaa"}))
    (is (= (set (filter (exact-count 7) test-data)) #{"aabbbbb" "aaabbbb" "aaaabbb" "aaaaabb"}))
    (is (= (set (filter (exact-count 10) test-data)) #{"aaaaabbbbb"}))

    (is (= (set (filter (count-between 3 10) test-data)) #{"bbb" "bbbb" "bbbbb"
                                                           "abb" "abbb" "abbbb" "abbbbb"
                                                           "aab" "aabb" "aabbb" "aabbbb" "aabbbbb"
                                                           "aaa" "aaab" "aaabb" "aaabbb" "aaabbbb" "aaabbbbb"
                                                           "aaaa" "aaaab" "aaaabb" "aaaabbb" "aaaabbbb" "aaaabbbbb"
                                                           "aaaaa" "aaaaab" "aaaaabb" "aaaaabbb" "aaaaabbbb" "aaaaabbbbb"}))
    (is (= (set (filter (count-between 5 10) test-data)) #{"bbbbb"
                                                           "abbbb" "abbbbb"
                                                           "aabbb" "aabbbb" "aabbbbb"
                                                           "aaabb" "aaabbb" "aaabbbb" "aaabbbbb"
                                                           "aaaab" "aaaabb" "aaaabbb" "aaaabbbb" "aaaabbbbb"
                                                           "aaaaa" "aaaaab" "aaaaabb" "aaaaabbb" "aaaaabbbb" "aaaaabbbbb"}))
    (is (= (set (filter (count-between 7 10) test-data)) #{"aabbbbb"
                                                           "aaabbbb" "aaabbbbb"
                                                           "aaaabbb" "aaaabbbb" "aaaabbbbb"
                                                           "aaaaabb" "aaaaabbb" "aaaaabbbb" "aaaaabbbbb"}))
    (is (= (set (filter (count-between 3 7) test-data)) #{"bbb" "bbbb" "bbbbb"
                                                          "abb" "abbb" "abbbb" "abbbbb"
                                                          "aab" "aabb" "aabbb" "aabbbb" "aabbbbb"
                                                          "aaa" "aaab" "aaabb" "aaabbb" "aaabbbb"
                                                          "aaaa" "aaaab" "aaaabb" "aaaabbb"
                                                          "aaaaa" "aaaaab" "aaaaabb"}))
    (is (= (set (filter (count-between 3 5) test-data)) #{"bbb" "bbbb" "bbbbb"
                                                          "abb" "abbb" "abbbb"
                                                          "aab" "aabb" "aabbb"
                                                          "aaa" "aaab" "aaabb"
                                                          "aaaa" "aaaab"
                                                          "aaaaa"}))
    (is (= (set (filter (count-between 5 7) test-data)) #{"bbbbb"
                                                          "abbbb" "abbbbb"
                                                          "aabbb" "aabbbb" "aabbbbb"
                                                          "aaabb" "aaabbb" "aaabbbb"
                                                          "aaaab" "aaaabb" "aaaabbb"
                                                          "aaaaa" "aaaaab" "aaaaabb"}))

    ))

(deftest test-check-value
  (let [test-data (vec (range 1 11))]

    (is (= (filterv (minimum 3) test-data) [3 4 5 6 7 8 9 10]))
    (is (= (filterv (minimum 5) test-data) [5 6 7 8 9 10]))
    (is (= (filterv (minimum 7) test-data) [7 8 9 10]))
    (is (= (filterv (minimum 10) test-data) [10]))

    (is (= (filterv (maximum 3) test-data) [1 2 3]))
    (is (= (filterv (maximum 5) test-data) [1 2 3 4 5]))
    (is (= (filterv (maximum 7) test-data) [1 2 3 4 5 6 7]))
    (is (= (filterv (maximum 10) test-data) [1 2 3 4 5 6 7 8 9 10]))

    (is (= (filterv (exact 3) test-data) [3]))
    (is (= (filterv (exact 5) test-data) [5]))
    (is (= (filterv (exact 7) test-data) [7]))
    (is (= (filterv (exact 10) test-data) [10]))

    (is (= (filterv (between 3 10) test-data) [3 4 5 6 7 8 9 10]))
    (is (= (filterv (between 5 10) test-data) [5 6 7 8 9 10]))
    (is (= (filterv (between 7 10) test-data) [7 8 9 10]))
    (is (= (filterv (between 3 7) test-data) [3 4 5 6 7]))
    (is (= (filterv (between 3 5) test-data) [3 4 5]))
    (is (= (filterv (between 5 7) test-data) [5 6 7]))

    ))

