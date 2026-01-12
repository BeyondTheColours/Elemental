let elements = ["h";"he";"li";"be";"b";"c";"n";"o";"f";"ne";"na";"mg";"al";"si";"p";"s";"cl";"ar";"k";"ca";"sc";"ti";"v";"cr";"mn";"fe";"co";"ni";"cu";"zn";"ga";"ge";"as";"se";"br";"kr";"rb";"sr";"y";"zr";"nb";"mo";"tc";"ru";"rh";"pd";"ag";"cd";"in";"sn";"sb";"te";"i";"xe";"cs";"ba";"la";"ce";"pr";"nd";"pm";"sm";"eu";"gd";"tb";"dy";"ho";"er";"tm";"yb";"lu";"hf";"ta";"w";"re";"os";"ir";"pt";"au";"hg";"tl";"pb";"bi";"po";"at";"rn";"fr";"ra";"ac";"th";"pa";"u";"np";"pu";"am";"cm";"bk";"cf";"es";"fm";"md";"lr"];;

let elements_of_string elements s =
  let rec aux elements s count =
    
    if count = String.length s then None
    else if count = (String.length s) - 2 then
      
      begin
        let last_two = (Char.escaped s.[count])^(Char.escaped s.[count+1]) in 
        let penultimate = Char.escaped s.[count] in
        
        match aux elements s (count+1) with
        |None -> if List.mem last_two elements then Some([[last_two]]) else None
        |Some(v) ->
            begin
              if List.mem penultimate elements && List.mem last_two elements then
                Some((List.map (fun l -> penultimate::l) v)@[[last_two]])
              else if List.mem penultimate elements then
                Some(List.map (fun l -> penultimate::l) v)
              else if List.mem last_two elements then
                Some([[last_two]])
              else None
            end
      end
      
    else if count = (String.length s) - 1 then
      begin 
        let last = Char.escaped s.[count] in
        if List.mem last elements then 
          Some([[last]])
        else None
      end
    else
      begin
        let first_is_element = List.mem (Char.escaped s.[count]) elements in
        let first_two = (Char.escaped s.[count])^(Char.escaped s.[count+1]) in
        let first_two_is_element = List.mem first_two elements in
          
        match (first_is_element, first_two_is_element) with
        |(false, false) -> None
        |(true, true) ->
            begin
              match ((aux elements s (count+1)), (aux elements s (count+2))) with
              |(None, None) -> None
              |(Some(v), None) -> let first_res = List.map (fun l -> (Char.escaped s.[count])::l) v in Some(first_res)
              |(None, Some(v)) -> let first_two_res = List.map (fun l -> (first_two)::l) v in Some(first_two_res)
              |(Some(v1), Some(v2)) ->
                  begin
                    let first_res = List.map (fun l -> (Char.escaped s.[count])::l) v1 in
                    let first_two_res = List.map (fun l -> (first_two)::l) v2 in
                    Some(first_res@first_two_res)
                  end
            end
        |(true, false) ->
            begin
              match aux elements s (count+1) with
              |None -> None
              |Some(v) -> let first_res = List.map (fun l -> (Char.escaped s.[count])::l) v in Some(first_res)
            end
        |(false, true) -> 
            begin
              match aux elements s (count+2) with
              |None -> None
              |Some(v) -> let first_two_res = List.map (fun l -> (first_two::l)) v in Some(first_two_res)
            end
      end
  in aux elements s 0
;;

let string_of_string_list l =
  let rec aux l acc =
    match l with
    |[] -> acc^"]"
    |h::[] -> acc^h^"]"
      |h::t -> aux t (acc^h^";")
  in aux l "["

let print_result input result =
  match result with
  |None ->
    print_endline ("\' "^input^" \'"^" can\'t be spelt using elements from the period table")
  |Some(v) ->
    print_endline ("\' "^input^" \'"^" can be spelt using elements as:");
    List.map string_of_string_list v |>
    List.iter print_endline
;;
