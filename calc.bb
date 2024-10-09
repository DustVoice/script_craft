(require '[clojure.math :as math])

(def recipes {"Dia" nil
              "Gold" nil
              "Iron" nil
              "Quartz" nil
              "Redstone" nil
              "Sand" nil
              "Slime" nil
              "String" nil
              "Glass" {:ingredients {"Sand" 1}}
              "Silicone" {:ingredients {"Quartz" 1}}
              "QEI" {:ingredients {"Iron" 3 "Quartz" 1} :yield 4}
              "Proc Binding" {:ingredients {"String" 2 "Slime" 1} :yield 8}
              "Raw Basic Proc" {:ingredients {"Iron" 1 "Redstone" 1 "Silicone" 1 "Proc Binding" 1}}
              "Raw Impr Proc" {:ingredients {"Gold" 1 "Redstone" 1 "Silicone" 1 "Proc Binding" 1}}
              "Raw Adv Proc" {:ingredients {"Dia" 1 "Redstone" 1 "Silicone" 1 "Proc Binding" 1}}
              "Basic Proc" {:ingredients {"Raw Basic Proc" 1}}
              "Impr Proc" {:ingredients {"Raw Impr Proc" 1}}
              "Adv Proc" {:ingredients {"Raw Adv Proc" 1}}
              "1k Part" {:ingredients {"Glass" 3 "Silicone" 4 "Redstone" 1 "QEI" 1}}
              "4k Part" {:ingredients {"1k Part" 3 "Basic Proc" 4 "Redstone" 1 "QEI" 1}}
              "16k Part" {:ingredients {"4k Part" 3 "Impr Proc" 4 "Redstone" 1 "QEI" 1}}
              "64k Part" {:ingredients {"16k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "256k Part" {:ingredients {"64k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "1024k Part" {:ingredients {"256k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "4096k Part" {:ingredients {"1024k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "16384k Part" {:ingredients {"4096k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}})

(declare cook)

(defn itemcount
  "Counts the total amount of ingredients (in the recipe) needed to produce the given item"
  [item]
  (let [recipe (get recipes item)
        atom {item 1}]
    (cond
      (nil? recipe) atom
      :else (let [cooked (cook (:ingredients recipe))
                  yield (:yield recipe)]
              (merge-with + atom (cond
                                   (nil? yield) cooked
                                   :else (update-vals cooked #(/ % yield))))))))

(defn cook
  "Calculates the total amount of items needed to fullfill a given item quantity list"
  [items]
  (reduce #(merge-with + %1 %2) (map
                                 (fn [[name quantity]] (update-vals (itemcount name) #(* % quantity)))
                                 items)))

(defn min-count [itemlist] (update-vals itemlist #(int (math/ceil %))))

(doseq [[name quantity] (sort-by val > (min-count (cook {"16384k Part", 1})))] (println name ":" quantity))
