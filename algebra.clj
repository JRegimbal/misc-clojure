(defn prime? [n]
  ((fn rec [n i]
     (if (< (Math/sqrt n) i) true
       (if (= (mod n i) 0) false
         (rec n (+ i 1))
         )))
   n 2))


; gcd(a, b, c) = gcd(a, gcd(b, c)), gcd(a, b) = gcd(b, a)
(defn gcd [& others]
  (reduce
          (fn f [r s]
            (if (= 0 s) r
              (f s (mod r s))))
          others))

; check if a in G for set G
(defn in [a G] (reduce #(or %1 %2) (map #(= a %) G)))

; modular addition (a + b + ...) (mod n)
(defn mod+ [n & args] (mod (reduce #(+ %1 %2) args) n))

; modular subtraction
(defn mod- [n & args] (mod (reduce #(- %1 %2) args) n))

; modular multiplication
(defn mod* [n & args] (mod (reduce #(* %1 %2) args) n))

(defn ident [operator G]
  ((fn f [e H]
    (if (reduce #(and %1 %2)
                (map #(= (operator % e) (operator e %) %) G))
      e
      (if (nil? H) nil
        (f (first H) (rest H)))))
  (first G) (rest G)))

(defn monoid? [operator G]
  (do
    ; closure (ab in G, ba in G for all a,b in G)
    (def closure
      (reduce #(and %1 %2) 
              (map (fn [a] 
                     (reduce #(and %1 %2) 
                             (map #(and (in (operator a %) G) (in (operator % a) G)) 
                                  G)))
                   G)))
    ; associativity (a(bc) = (ab)c for all a,b,c in G)
    (def associativity
      (reduce #(and %1 %2)
              (map (fn [a]
                     (reduce #(and %1 %2)
                             (map (fn [b]
                                    (reduce #(and %1 %2)
                                            (map #(= (operator a (operator b %)) (operator (operator a b) %)) G)))
                                  G)))
                   G)))
    ; identity (there is some e in G such that ae = ea = a for all a in G)
    (= closure associativity (some? (ident operator G)))))



(defn group? [operator G]
  (do
    ; closure (ab in G, ba in G for all a,b in G)
    ; associativity (a(bc) = (ab)c for all a,b,c in G)
    ; identity (there is some e in G such that ae = ea = a for all a in G)
    ; inverse (for all a in G there is some b in G such that ab = ba = e
    (def e (ident operator G))
    (if (not (monoid? operator G)) false
      (if (nil? e) false ; if there's no identity element then there can't be an inverse
        (reduce #(and %1 %2)
                (map (fn [a]
                       (reduce #(or %1 %2)
                               (map #(= (operator a %) (operator % a) e) G)))
                     G))))))

; check if the operator and set define an abelian group
(defn abelian? [operator G]
  (if (not (group? operator G)) false
    (reduce #(and %1 %2)
            (map (fn [a]
                   (reduce #(and %1 %2)
                           (map #(= (operator a %) (operator % a)) G)))
                 G))))

