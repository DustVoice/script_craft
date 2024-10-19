(require '[clojure.math :as math])

(def recipes {"Dia" nil
              "Gold" nil
              "Iron" nil
              "Lithium Dust" nil
              "Osmium" nil
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
              "16384k Part" {:ingredients {"4096k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "Gravitational Modulating Unit" {:ingredients {"Atomic Alloy" 2
                                                             "Nether Star" 1
                                                             "Ultimate Induction Provider" 2
                                                             "Module Base" 1
                                                             "Antimatter Pellet" 3}}
              "Atomic Alloy" {:ingredients {"Reinforced Alloy" 1 "Refined Obsidian Dust" 1}}
              "Refined Obsidian Dust" {:ingredients {"Obsidian Dust" 1 "Diamond Dust" 1}}
              "Reinforced Alloy" {:ingredients {"Infused Alloy" 1 "Diamond Dust" 1}}
              "Infused Alloy" {:ingredients {"Iron" 1 "Redstone" 1}}
              "Module Base" {:ingredients {"Tin Ingot" 4 "Bronze Nugget" 4 "HDPE Sheet" 1} :yield 2}
              "Energy Tablet" {:ingredients {"Redstone" 4 "Infused Alloy" 2 "Gold" 3}}
              "Ultimate Control Circuit" {:ingredients {"Atomic Alloy" 2 "Elite Control Circuit" 1}}
              "Elite Control Circuit" {:ingredients {"Reinforced Alloy" 2 "Advanced Control Circuit" 1}}
              "Advanced Control Circuit" {:ingredients {"Infused Alloy" 2 "Basic Control Circuit" 1}}
              "Basic Control Circuit" {:ingredients {"Osmium" 1 "Redstone" 1}}
              "Ultimate Energy Cube" {:ingredients {"Atomic Alloy" 4 "Diamond" 2 "Energy Tablet" 2 "Elite Energy Cube" 1}}
              "Elite Energy Cube" {:ingredients {"Reinforced Alloy" 4 "Gold" 2 "Energy Tablet" 2 "Advanced Energy Cube" 1}}
              "Advanced Energy Cube" {:ingredients {"Infused Alloy" 4 "Osmium" 2 "Energy Tablet" 2 "Basic Energy Cube" 1}}
              "Basic Energy Cube" {:ingredients {"Redstone" 4 "Iron" 2 "Energy Tablet" 2 "Steel Casing" 1}}
              "Steel Casing" {:ingredients {"Steel" 4 "Glass" 4 "Osmium" 1}}
              "Ultimate Induction Provider" {:ingredients {"Ultimate Control Circuit" 4 "Elite Induction Provider" 4 "Ultimate Energy Cube" 1}}
              "Elite Induction Provider" {:ingredients {"Elite Control Circuit" 4 "Advanced Induction Provider" 4 "Elite Energy Cube" 1}}
              "Advanced Induction Provider" {:ingredients {"Advanced Control Circuit" 4 "Basic Induction Provider" 4 "Advanced Energy Cube" 1}}
              "Basic Induction Provider" {:ingredients {"Basic Control Circuit" 4 "Lithium Dust" 4 "Basic Energy Cube" 1}}
              })

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

(println "Refined Storage: 16384k Part")
(doseq [[name quantity] (sort-by val > (min-count (cook {"16384k Part", 1})))] (println name ":" quantity))

(println "=========")

(println "Mekanism: Gravitational Modulating Unit")
(doseq [[name quantity] (sort-by val > (min-count (cook {"Gravitational Modulating Unit", 1})))] (println name ":" quantity))
