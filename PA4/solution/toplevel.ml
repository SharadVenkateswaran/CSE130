let ts = List.map (fun s -> "tests/"^s)
  ["t1.ml";"t2.ml";"t3.ml";"t4.ml";"t5.ml";"t7.ml";"t8.ml";
  "t9.ml";"t10.ml";"t11.ml";"t12.ml";"t13.ml";"t14.ml"]
  ;;

List.map Main.execute ts ;;











