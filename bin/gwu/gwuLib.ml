(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Def
open Gwdb

let isolated = ref false
let old_gw = ref false
let only_file = ref ""
let out_dir = ref ""
let raw_output = ref false
let sep_limit = ref 21
let separate_list = ref []

let put_events_in_notes base p =
  (* Si on est en mode old_gw, on mets tous les évènements *)
  (* dans les notes.                                       *)
  if !old_gw then
    let rec loop pevents =
      match pevents with
      | [] -> false
      | evt :: events ->
        match evt.epers_name with
          Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial |
          Epers_Cremation ->
          if sou base evt.epers_note <> "" ||
             evt.epers_witnesses <> [| |]
          then
            true
          else loop events
        | _ -> true
    in
    loop (get_pevents p)
  else false

let ht_dup_occ = Hashtbl.create 20001

let ht_orig_occ = Hashtbl.create 20001

let prepare_free_occ ?(select = fun _ -> true) base =
  (* Parce qu'on est obligé ... *)
  let sn = "?" in
  let fn = "?" in
  let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
  Hashtbl.add ht_orig_occ key [0] ;
  Gwdb.Collection.iter begin fun ip ->
    if select ip then
      let p = poi base ip in
      let sn = sou base (get_surname p) in
      let fn = sou base (get_first_name p) in
      if sn = "?" && fn = "?" then ()
      else
        let fn = Name.lower fn in
        let sn = Name.lower sn in
        if fn = "" || sn = "" then
          let key = fn ^ " #@# " ^ sn in
          let occ = get_occ p in
          try
            let l = Hashtbl.find ht_orig_occ key in
            if List.mem occ l then Hashtbl.add ht_dup_occ ip occ
            else Hashtbl.replace ht_orig_occ key (occ :: l)
          with Not_found -> Hashtbl.add ht_orig_occ key [occ]
  end (Gwdb.ipers base) ;
  Hashtbl.iter
    (fun key l -> Hashtbl.replace ht_orig_occ key (List.sort compare l))
    ht_orig_occ;
  Hashtbl.iter begin fun ip _ ->
    let p = poi base ip in
    let sn = sou base (get_surname p) in
    let fn = sou base (get_first_name p) in
    let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
    try
      let list_occ = Hashtbl.find ht_orig_occ key in
      let rec loop list init new_list =
        match list with
        | x :: y :: l when y - x > 1 ->
          succ x, List.rev_append (y :: succ x :: x :: new_list) l
        | x :: l -> loop l (succ x) (x :: new_list)
        | [] -> init, [init]
      in
      let (new_occ, new_list_occ) = loop list_occ 0 [] in
      Hashtbl.replace ht_dup_occ ip new_occ;
      Hashtbl.replace ht_orig_occ key new_list_occ
    with Not_found -> ()
  end ht_dup_occ

let get_new_occ p =
  try Hashtbl.find ht_dup_occ (get_iper p)
  with Not_found -> get_occ p

type mfam =
  { m_ifam : ifam;
    m_fam : family;
    m_fath : person;
    m_moth : person;
    m_chil : person array }

let soy y = if y = 0 then "-0" else string_of_int y

let print_date_dmy oc d =
  begin match d.prec with
      About -> Printf.ksprintf oc "~"
    | Maybe -> Printf.ksprintf oc "?"
    | Before -> Printf.ksprintf oc "<"
    | After -> Printf.ksprintf oc ">"
    | _ -> ()
  end;
  if d.month = 0 then Printf.ksprintf oc "%s" (soy d.year)
  else if d.day = 0 then Printf.ksprintf oc "%d/%s" d.month (soy d.year)
  else Printf.ksprintf oc "%d/%d/%s" d.day d.month (soy d.year);
  match d.prec with
    OrYear d2 ->
    if not !old_gw then
      if d2.month2 = 0 then Printf.ksprintf oc "|%s" (soy d2.year2)
      else if d2.day2 = 0 then
        Printf.ksprintf oc "|%d/%s" d2.month2 (soy d2.year2)
      else Printf.ksprintf oc "|%d/%d/%s" d2.day2 d2.month2 (soy d2.year2)
    else Printf.ksprintf oc "|%s" (soy d2.year2)
  | YearInt d2 ->
    if not !old_gw then
      if d2.month2 = 0 then Printf.ksprintf oc "..%s" (soy d2.year2)
      else if d2.day2 = 0 then
        Printf.ksprintf oc "..%d/%s" d2.month2 (soy d2.year2)
      else Printf.ksprintf oc "..%d/%d/%s" d2.day2 d2.month2 (soy d2.year2)
    else Printf.ksprintf oc "..%s" (soy d2.year2)
  | _ -> ()

let is_printable =
  function
  | '\000'..'\031' -> false
  | _ -> true

let starting_char no_num s =
  match s.[0] with
  (*'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' *)
  | 'a'..'z' | 'A'..'Z' | '\xE0'..'\xFD' | '\xC0'..'\xDD' -> true
  | '0'..'9' -> not no_num
  | '?' -> if s = "?" then true else false
  | _ -> false

let no_newlines s =
  let conv_char i =
    match s.[i] with
    | '\n' | '\r' -> ' '
    | _ -> s.[i]
  in
  String.init (String.length s) conv_char

let gen_correct_string no_num no_colon s =
  let s = String.trim s in
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if len = 0 && not (starting_char no_num s) then
      loop i (Buff.store len '_')
    else
      match s.[i] with
      | ' ' | '\n' | '\t' ->
        if i = String.length s - 1 then Buff.get len
        else loop (i + 1) (Buff.store len '_')
      | '_' | '\\' ->
        loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | ':' when no_colon ->
        let len = Buff.store len '\\' in
        loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | c ->
        let c = if is_printable c then c else '_' in
        loop (i + 1) (Buff.store len c)
  in
  loop 0 0

let s_correct_string s =
  let s = gen_correct_string false false s in if s = "" then "_" else s

let s_correct_string_nonum s =
  let s = gen_correct_string true false s in if s = "" then "_" else s

let correct_string base is = s_correct_string (sou base is)

let correct_string_no_colon base is =
  gen_correct_string false true (sou base is)

let gen_print_date no_colon oc =
  function
    Dgreg (d, Dgregorian) -> print_date_dmy oc d
  | Dgreg (d, Djulian) ->
    print_date_dmy oc (Calendar.julian_of_gregorian d); Printf.ksprintf oc "J"
  | Dgreg (d, Dfrench) ->
    print_date_dmy oc (Calendar.french_of_gregorian d); Printf.ksprintf oc "F"
  | Dgreg (d, Dhebrew) ->
    print_date_dmy oc (Calendar.hebrew_of_gregorian d); Printf.ksprintf oc "H"
  | Dtext t ->
    (* Dans le cas d'une date texte pour un titre, on échappe les ':' *)
    let t = gen_correct_string false no_colon t in Printf.ksprintf oc "0(%s)" t

let gen_print_date_option no_colon oc =
  function
    Some d -> gen_print_date no_colon oc d
  | None -> ()

let print_date oc = gen_print_date false oc

let print_date_option oc = gen_print_date_option false oc

let print_title_date_option oc = gen_print_date_option true oc

let lines_list_of_string s =
  let rec loop lines len i =
    if i = String.length s then
      List.rev (if len = 0 then lines else Buff.get len :: lines)
    else if s.[i] = '\n' then
      let line = Buff.get len in loop (line :: lines) 0 (i + 1)
    else loop lines (Buff.store len s.[i]) (i + 1)
  in
  loop [] 0 0

let has_infos_not_dates base p =
  let has_picture_to_export =
    sou base (get_image p) <> "" && not Gwexport.(!opts.no_picture)
  in
  get_first_names_aliases p <> []
  || get_surnames_aliases p <> []
  || sou base (get_public_name p) <> ""
  || has_picture_to_export
  || get_qualifiers p <> []
  || get_aliases p <> []
  || get_titles p <> []
  || get_access p <> IfTitles
  || sou base (get_occupation p) <> ""
  || (Gwexport.(!opts.source) <> None || sou base @@ get_psources p <> "")
  || sou base (get_birth_place p) <> ""
  || (Gwexport.(!opts.source) = None && sou base (get_birth_src p) <> "")
  || sou base (get_baptism_place p) <> ""
  || (Gwexport.(!opts.source) = None && sou base (get_baptism_src p) <> "")
  || sou base (get_death_place p) <> ""
  || (Gwexport.(!opts.source) = None && sou base (get_death_src p) <> "")
  || sou base (get_burial_place p) <> ""
  || (Gwexport.(!opts.source) = None && sou base (get_burial_src p) <> "")

let has_infos base p =
  has_infos_not_dates base p
  || get_birth p <> Adef.cdate_None
  || get_baptism p <> Adef.cdate_None
  || get_death p <> NotDead

let print_if_not_equal_to x oc base lab is =
  if sou base is = x then ()
  else Printf.ksprintf oc " %s %s" lab (correct_string base is)

let print_src_if_not_equal_to x oc base lab is =
  match Gwexport.(!opts.source) with
  | None -> if sou base is <> "" then print_if_not_equal_to x oc base lab is
  | Some x -> Printf.ksprintf oc " %s %s" lab (s_correct_string x)

let print_if_no_empty = print_if_not_equal_to ""

let print_src_if_no_empty oc base lab is =
  if Gwexport.(!opts.source) = None
  then print_if_not_equal_to "" oc base lab is

let print_if_no_empty_endline oc base lab is =
  if sou base is = "" then ()
  else Printf.ksprintf oc " %s %s\n" lab (correct_string base is)

let print_if_no_empty_no_newline oc base lab is =
  if sou base is = "" then ()
  else Printf.ksprintf oc " %s %s" lab (no_newlines (correct_string base is))

let print_first_name_alias oc base is =
  Printf.ksprintf oc " {%s}" (correct_string base is)

let print_surname_alias oc base is =
  Printf.ksprintf oc " #salias %s" (correct_string base is)

let print_qualifier oc base is =
  Printf.ksprintf oc " #nick %s" (correct_string base is)

let print_alias oc base is =
  Printf.ksprintf oc " #alias %s" (correct_string base is)

let print_burial oc b =
  match b with
    Buried cod ->
    Printf.ksprintf oc " #buri";
    begin match Adef.od_of_cdate cod with
        Some d -> Printf.ksprintf oc " "; print_date oc d; ()
      | _ -> ()
    end
  | Cremated cod ->
    Printf.ksprintf oc " #crem";
    begin match Adef.od_of_cdate cod with
        Some d -> Printf.ksprintf oc " "; print_date oc d; ()
      | _ -> ()
    end
  | UnknownBurial -> ()

let print_title oc base t =
  let t_date_start = Adef.od_of_cdate t.t_date_start in
  let t_date_end = Adef.od_of_cdate t.t_date_end in
  Printf.ksprintf oc " [";
  begin match t.t_name with
      Tmain -> Printf.ksprintf oc "*"
    | Tname s -> Printf.ksprintf oc "%s" (correct_string_no_colon base s)
    | Tnone -> ()
  end;
  Printf.ksprintf oc ":";
  Printf.ksprintf oc "%s" (correct_string_no_colon base t.t_ident);
  Printf.ksprintf oc ":";
  Printf.ksprintf oc "%s" (correct_string_no_colon base t.t_place);
  if t.t_nth <> 0 then Printf.ksprintf oc ":"
  else
    begin match t_date_start, t_date_end with
        Some _, _ | _, Some _ -> Printf.ksprintf oc ":"
      | _ -> ()
    end;
  print_title_date_option oc t_date_start;
  if t.t_nth <> 0 then Printf.ksprintf oc ":"
  else
    begin match t_date_end with
        Some _ -> Printf.ksprintf oc ":"
      | _ -> ()
    end;
  print_title_date_option oc t_date_end;
  if t.t_nth <> 0 then Printf.ksprintf oc ":%d" t.t_nth;
  Printf.ksprintf oc "]"

let zero_birth_is_required base is_child p =
  if get_baptism p <> Adef.cdate_None then false
  else
    begin match get_death p with
        Death (_, _) | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
        true
      | DontKnowIfDead
        when
          not is_child && not (has_infos_not_dates base p) &&
          p_first_name base p <> "?" && p_surname base p <> "?" ->
        true
      | _ -> false
    end

let print_infos oc base is_child csrc cbp p =
  List.iter (print_first_name_alias oc base) (get_first_names_aliases p);
  List.iter (print_surname_alias oc base) (get_surnames_aliases p);
  begin match get_public_name p with
      s when sou base s <> "" -> Printf.ksprintf oc " (%s)" (correct_string base s)
    | _ -> ()
  end;
  if not Gwexport.(!opts.no_picture) then
    print_if_no_empty oc base "#image" (get_image p);
  List.iter (print_qualifier oc base) (get_qualifiers p);
  List.iter (print_alias oc base) (get_aliases p);
  List.iter (print_title oc base) (get_titles p);
  begin match get_access p with
      IfTitles -> ()
    | Public -> Printf.ksprintf oc " #apubl"
    | Private -> Printf.ksprintf oc " #apriv"
  end;
  print_if_no_empty oc base "#occu" (get_occupation p);
  print_src_if_not_equal_to csrc oc base "#src" (get_psources p);
  begin match Adef.od_of_cdate (get_birth p) with
      Some d -> Printf.ksprintf oc " "; print_date oc d
    | _ when zero_birth_is_required base is_child p -> Printf.ksprintf oc " 0"
    | _ -> ()
  end;
  print_if_not_equal_to cbp oc base "#bp" (get_birth_place p);
  if Gwexport.(!opts.source) = None then
    print_if_no_empty oc base "#bs" (get_birth_src p);
  begin match Adef.od_of_cdate (get_baptism p) with
      Some d -> Printf.ksprintf oc " !"; print_date oc d
    | _ -> ()
  end;
  print_if_no_empty oc base "#pp" (get_baptism_place p);
  if Gwexport.(!opts.source) = None then
    print_if_no_empty oc base "#ps" (get_baptism_src p);
  begin match get_death p with
      Death (dr, d) ->
      Printf.ksprintf oc " ";
      begin match dr with
          Killed -> Printf.ksprintf oc "k"
        | Murdered -> Printf.ksprintf oc "m"
        | Executed -> Printf.ksprintf oc "e"
        | Disappeared -> Printf.ksprintf oc "s"
        | _ -> ()
      end;
      print_date oc (Adef.date_of_cdate d)
    | DeadYoung -> Printf.ksprintf oc " mj"
    | DeadDontKnowWhen -> Printf.ksprintf oc " 0"
    | DontKnowIfDead ->
      begin match
          Adef.od_of_cdate (get_birth p), Adef.od_of_cdate (get_baptism p)
        with
          Some _, _ | _, Some _ -> Printf.ksprintf oc " ?"
        | _ -> ()
      end
    | OfCourseDead -> Printf.ksprintf oc " od"
    | NotDead -> ()
  end;
  print_if_no_empty oc base "#dp" (get_death_place p);
  if Gwexport.(!opts.source) = None then
    print_if_no_empty oc base "#ds" (get_death_src p);
  print_burial oc (get_burial p);
  print_if_no_empty oc base "#rp" (get_burial_place p);
  if Gwexport.(!opts.source) = None then
    print_if_no_empty oc base "#rs" (get_burial_src p)

type gen =
  { mark : (iper, bool) Gwdb.Marker.t;
    mark_rel : (iper, bool) Gwdb.Marker.t;
    per_sel : iper -> bool;
    fam_sel : ifam -> bool;
    fam_done : (ifam, bool) Gwdb.Marker.t;
    mutable notes_pl_p : person list;
    mutable ext_files : (string * string list ref) list;
    mutable notes_alias : (string * string) list;
    mutable pevents_pl_p : person list }

let map_notes aliases f = try List.assoc f aliases with Not_found -> f

let add_linked_files gen from s some_linked_files =
  let slen = String.length s in
  let rec loop new_linked_files i =
    if i = slen then new_linked_files
    else if
      i < slen - 2 && s.[i] = '[' && s.[i+1] = '[' && s.[i+2] = '['
    then
      let j =
        let rec loop j =
          if j = slen then j
          else if
            j < slen - 2 && s.[j] = ']' && s.[j+1] = ']' && s.[j+2] = ']'
          then
            j + 3
          else loop (j + 1)
        in
        loop (i + 3)
      in
      if j > i + 6 then
        let b = String.sub s (i + 3) (j - i - 6) in
        let fname =
          try let k = String.index b '/' in String.sub b 0 k with
            Not_found -> b
        in
        let fname = map_notes gen.notes_alias fname in
        let f = from () in
        let new_linked_files =
          try
            let r = List.assoc fname gen.ext_files in
            if List.mem f !r then () else r := f :: !r; new_linked_files
          with Not_found ->
            let lf = fname, ref [f] in
            gen.ext_files <- lf :: gen.ext_files; lf :: new_linked_files
        in
        loop new_linked_files j
      else loop new_linked_files (i + 1)
    else loop new_linked_files (i + 1)
  in
  loop some_linked_files 0

let find_free_occ base f s =
  let ipl = persons_of_name base (f ^ " " ^ s) in
  let first_name = f in
  let surname = s in
  let list_occ =
    let rec loop list =
      function
        ip :: ipl ->
        let p = poi base ip in
        if not (List.mem (get_occ p) list) &&
           first_name = p_first_name base p &&
           surname = p_surname base p
        then
          loop (get_occ p :: list) ipl
        else loop list ipl
      | [] -> list
    in
    loop [] ipl
  in
  let list_occ = List.sort compare list_occ in
  let rec loop cnt1 =
    function
      cnt2 :: list -> if cnt1 = cnt2 then loop (cnt1 + 1) list else cnt1
    | [] -> cnt1
  in
  loop 0 list_occ

let print_parent oc base gen p =
  let has_printed_parents =
    match get_parents p with
      Some ifam -> gen.fam_sel ifam
    | None -> false
  in
  let first_parent_definition =
    if Gwdb.Marker.get gen.mark (get_iper p) then false
    else
      begin
        Gwdb.Marker.set gen.mark (get_iper p) true;
        true
      end
  in
  let pr = not has_printed_parents && first_parent_definition in
  let has_infos = if pr then has_infos base p else false in
  let first_name = sou base (get_first_name p) in
  let surname = sou base (get_surname p) in
  Printf.ksprintf oc "%s %s%s" (s_correct_string surname)
    (s_correct_string first_name)
    (if first_name = "?" && surname = "?" then ""
     else if get_new_occ p = 0 then ""
     else "." ^ string_of_int (get_new_occ p));
  if pr then
    if has_infos then print_infos oc base false "" "" p
    else if first_name <> "?" && surname <> "?" then Printf.ksprintf oc " 0"

let print_child oc base fam_surname csrc cbp p =
  Printf.ksprintf oc "-";
  begin match get_sex p with
      Male -> Printf.ksprintf oc " h"
    | Female -> Printf.ksprintf oc " f"
    | _ -> ()
  end;
  Printf.ksprintf oc " %s" (s_correct_string (sou base (get_first_name p)));
  if p_first_name base p = "?" && p_surname base p = "?" then ()
  else if get_new_occ p = 0 then ()
  else Printf.ksprintf oc ".%d" (get_new_occ p);
  if not (eq_istr (get_surname p) fam_surname) then
    Printf.ksprintf oc " %s" (s_correct_string_nonum (sou base (get_surname p)));
  print_infos oc base true csrc cbp p;
  Printf.ksprintf oc "\n"

let bogus_person base p =
  p_first_name base p = "?" && p_surname base p = "?"

let common_children proj base children =
  if Array.length children <= 1 then None
  else
    let list =
      List.map (fun p -> sou base (proj p)) (Array.to_list children)
    in
    if List.mem "" list then None
    else
      let list = List.sort compare list in
      let (src_max, n_max, _, _) =
        List.fold_left
          (fun (src_max, n_max, prev_src, n) src ->
             if src = prev_src then
               let n = n + 1 in
               if n > n_max then src, n, src, n
               else src_max, n_max, src, n
             else src_max, n_max, src, 1)
          ("", 0, "", 0) list
      in
      if n_max > 1 then Some src_max else None

let common_children_sources = common_children get_psources

let common_children_birth_place = common_children get_birth_place

let empty_family base m =
  bogus_person base m.m_fath && bogus_person base m.m_moth &&
  Array.for_all (bogus_person base) m.m_chil

let print_witness oc base gen p =
  Printf.ksprintf oc "%s %s%s" (correct_string base (get_surname p))
    (correct_string base (get_first_name p))
    (if get_new_occ p = 0 then ""
     else "." ^ string_of_int (get_new_occ p));
  if Array.length (get_family p) = 0 && get_parents p = None
     && not (Gwdb.Marker.get gen.mark (get_iper p))
  then
    begin
      Gwdb.Marker.set gen.mark (get_iper p) true;
      if has_infos base p then print_infos oc base false "" "" p
      else Printf.ksprintf oc " 0";
      begin match sou base (get_notes p) with
          "" ->
          if put_events_in_notes base p then
            gen.notes_pl_p <- p :: gen.notes_pl_p
        | _ -> gen.notes_pl_p <- p :: gen.notes_pl_p
      end;
      if get_pevents p <> [] then
        gen.pevents_pl_p <- p :: gen.pevents_pl_p
    end

let print_pevent oc base gen e =
  begin match e.epers_name with
      Epers_Birth -> Printf.ksprintf oc "#birt"
    | Epers_Baptism -> Printf.ksprintf oc "#bapt"
    | Epers_Death -> Printf.ksprintf oc "#deat"
    | Epers_Burial -> Printf.ksprintf oc "#buri"
    | Epers_Cremation -> Printf.ksprintf oc "#crem"
    | Epers_Accomplishment -> Printf.ksprintf oc "#acco"
    | Epers_Acquisition -> Printf.ksprintf oc "#acqu"
    | Epers_Adhesion -> Printf.ksprintf oc "#adhe"
    | Epers_BaptismLDS -> Printf.ksprintf oc "#bapl"
    | Epers_BarMitzvah -> Printf.ksprintf oc "#barm"
    | Epers_BatMitzvah -> Printf.ksprintf oc "#basm"
    | Epers_Benediction -> Printf.ksprintf oc "#bles"
    | Epers_ChangeName -> Printf.ksprintf oc "#chgn"
    | Epers_Circumcision -> Printf.ksprintf oc "#circ"
    | Epers_Confirmation -> Printf.ksprintf oc "#conf"
    | Epers_ConfirmationLDS -> Printf.ksprintf oc "#conl"
    | Epers_Decoration -> Printf.ksprintf oc "#awar"
    | Epers_DemobilisationMilitaire -> Printf.ksprintf oc "#demm"
    | Epers_Diploma -> Printf.ksprintf oc "#degr"
    | Epers_Distinction -> Printf.ksprintf oc "#dist"
    | Epers_Dotation -> Printf.ksprintf oc "#endl"
    | Epers_DotationLDS -> Printf.ksprintf oc "#dotl"
    | Epers_Education -> Printf.ksprintf oc "#educ"
    | Epers_Election -> Printf.ksprintf oc "#elec"
    | Epers_Emigration -> Printf.ksprintf oc "#emig"
    | Epers_Excommunication -> Printf.ksprintf oc "#exco"
    | Epers_FamilyLinkLDS -> Printf.ksprintf oc "#flkl"
    | Epers_FirstCommunion -> Printf.ksprintf oc "#fcom"
    | Epers_Funeral -> Printf.ksprintf oc "#fune"
    | Epers_Graduate -> Printf.ksprintf oc "#grad"
    | Epers_Hospitalisation -> Printf.ksprintf oc "#hosp"
    | Epers_Illness -> Printf.ksprintf oc "#illn"
    | Epers_Immigration -> Printf.ksprintf oc "#immi"
    | Epers_ListePassenger -> Printf.ksprintf oc "#lpas"
    | Epers_MilitaryDistinction -> Printf.ksprintf oc "#mdis"
    | Epers_MilitaryPromotion -> Printf.ksprintf oc "#mpro"
    | Epers_MilitaryService -> Printf.ksprintf oc "#mser"
    | Epers_MobilisationMilitaire -> Printf.ksprintf oc "#mobm"
    | Epers_Naturalisation -> Printf.ksprintf oc "#natu"
    | Epers_Occupation -> Printf.ksprintf oc "#occu"
    | Epers_Ordination -> Printf.ksprintf oc "#ordn"
    | Epers_Property -> Printf.ksprintf oc "#prop"
    | Epers_Recensement -> Printf.ksprintf oc "#cens"
    | Epers_Residence -> Printf.ksprintf oc "#resi"
    | Epers_Retired -> Printf.ksprintf oc "#reti"
    | Epers_ScellentChildLDS -> Printf.ksprintf oc "#slgc"
    | Epers_ScellentParentLDS -> Printf.ksprintf oc "#slgp"
    | Epers_ScellentSpouseLDS -> Printf.ksprintf oc "#slgs"
    | Epers_VenteBien -> Printf.ksprintf oc "#vteb"
    | Epers_Will -> Printf.ksprintf oc "#will"
    | Epers_Name s -> Printf.ksprintf oc "#%s" (correct_string base s)
  end;
  Printf.ksprintf oc " ";
  let epers_date = Adef.od_of_cdate e.epers_date in
  print_date_option oc epers_date;
  print_if_no_empty oc base "#p" e.epers_place;
  (* TODO *)
  (*print_if_no_empty oc base "#c" e.epers_cause;*)
  if Gwexport.(!opts.source) = None then
    print_if_no_empty oc base "#s" e.epers_src;
  Printf.ksprintf oc "\n";
  Array.iter
    (fun (ip, wk) ->
       if gen.per_sel ip then
         let p = poi base ip in
         Printf.ksprintf oc "wit";
         begin match get_sex p with
             Male -> Printf.ksprintf oc " m"
           | Female -> Printf.ksprintf oc " f"
           | _ -> ()
         end;
         Printf.ksprintf oc ": ";
         begin match wk with
             Witness_GodParent -> Printf.ksprintf oc "#godp "
           | _ -> ()
         end;
         print_witness oc base gen p;
         Printf.ksprintf oc "\n")
    e.epers_witnesses;
  let note = sou base e.epers_note in
  if note <> "" then
    List.iter (fun line -> Printf.ksprintf oc "note %s\n" line)
      (lines_list_of_string note)

let get_persons_with_pevents m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match get_pevents fath, get_parents fath with
      [], _ | _, Some _ -> list
    | _ -> fath :: list
  in
  let list =
    match get_pevents moth, get_parents moth with
      [], _ | _, Some _ -> list
    | _ -> moth :: list
  in
  Array.fold_right
    (fun p list -> if get_pevents p =  [] then list else p :: list)
    m.m_chil list

let print_pevents_for_person oc base gen p =
  let pevents = get_pevents p in
  let surn = s_correct_string (p_surname base p) in
  let fnam = s_correct_string (p_first_name base p) in
  if pevents <> [] && surn <> "?" && fnam <> "?" then
    begin
      Printf.ksprintf oc "\n";
      Printf.ksprintf oc "pevt %s %s%s\n" surn fnam
        (if get_new_occ p = 0 then ""
         else "." ^ string_of_int (get_new_occ p));
      List.iter (print_pevent oc base gen) pevents;
      Printf.ksprintf oc "end pevt\n"
    end

let rec list_memf f x =
  function
    [] -> false
  | a :: l -> f x a || list_memf f x l

let eq_key p1 p2 = get_iper p1 = get_iper p2

let eq_key_fst (p1, _) (p2, _) = get_iper p1 = get_iper p2

let print_pevents oc base gen ml =
  let pl =
    List.fold_right get_persons_with_pevents ml gen.pevents_pl_p
  in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else p :: pl) pl []
  in
  List.iter
    (fun p ->
       if gen.per_sel (get_iper p) then
         print_pevents_for_person oc base gen p)
    pl

let print_fevent oc base gen in_comment e =
  let print_sep () =
    if not in_comment then Printf.ksprintf oc "\n" else Printf.ksprintf oc " "
  in
  begin match e.efam_name with
      Efam_Marriage -> Printf.ksprintf oc "#marr"
    | Efam_NoMarriage -> Printf.ksprintf oc "#nmar"
    | Efam_NoMention -> Printf.ksprintf oc "#nmen"
    | Efam_Engage -> Printf.ksprintf oc "#enga"
    | Efam_Divorce -> Printf.ksprintf oc "#div"
    | Efam_Separated -> Printf.ksprintf oc "#sep"
    | Efam_Annulation -> Printf.ksprintf oc "#anul"
    | Efam_MarriageBann -> Printf.ksprintf oc "#marb"
    | Efam_MarriageContract -> Printf.ksprintf oc "#marc"
    | Efam_MarriageLicense -> Printf.ksprintf oc "#marl"
    | Efam_PACS -> Printf.ksprintf oc "#pacs"
    | Efam_Residence -> Printf.ksprintf oc "#resi"
    | Efam_Name n -> Printf.ksprintf oc "#%s" (correct_string base n)
  end;
  Printf.ksprintf oc " ";
  let efam_date = Adef.od_of_cdate e.efam_date in
  print_date_option oc efam_date;
  print_if_no_empty oc base "#p" e.efam_place;
  (*print_if_no_empty oc base "#c" e.efam_cause;*)
  if Gwexport.(!opts.source) = None then
    print_if_no_empty oc base "#s" e.efam_src;
  print_sep ();
  Array.iter
    (fun (ip, wk) ->
       if gen.per_sel ip then
         let p = poi base ip in
         Printf.ksprintf oc "wit";
         begin match get_sex p with
             Male -> Printf.ksprintf oc " m"
           | Female -> Printf.ksprintf oc " f"
           | _ -> ()
         end;
         Printf.ksprintf oc ": ";
         begin match wk with
             Witness_GodParent -> Printf.ksprintf oc "#godp "
           | _ -> ()
         end;
         print_witness oc base gen p;
         print_sep ())
    e.efam_witnesses;
  let note = sou base e.efam_note in
  if note <> "" then
    List.iter (fun line -> Printf.ksprintf oc "note %s" line; print_sep ())
      (lines_list_of_string note)

let print_comment_for_family oc base gen fam =
  let comm = sou base (get_comment fam) in
  (* Si on est en mode old_gw, on mets tous les évènements dans les notes. *)
  (* On supprime les 2 évènements principaux. *)
  let fevents =
    List.filter
      (fun evt ->
         match evt.efam_name with
           Efam_Divorce | Efam_Engage | Efam_Marriage | Efam_NoMarriage |
           Efam_NoMention | Efam_Separated ->
           false
         | _ -> true)
      (get_fevents fam)
  in
  let has_evt =
    !old_gw && (fevents <> [] || sou base (get_marriage_note fam) <> "")
  in
  if comm <> "" || has_evt then
    begin
      Printf.ksprintf oc "comm";
      if comm <> "" then Printf.ksprintf oc " %s" (no_newlines comm);
      if !old_gw then
        begin
          if sou base (get_marriage_note fam) <> "" then
            Printf.ksprintf oc " marriage: %s"
              (no_newlines (sou base (get_marriage_note fam)));
          List.iter
            (fun e -> Printf.ksprintf oc " "; print_fevent oc base gen true e)
            fevents
        end;
      Printf.ksprintf oc "\n"
    end

let print_empty_family oc base p =
  let string_quest = Gwdb.insert_string base "?" in
  Printf.ksprintf oc "fam ? ?.0 + #noment ? ?.0\n";
  Printf.ksprintf oc "beg\n";
  print_child oc base string_quest "" "" p;
  Printf.ksprintf oc "end\n"

let print_family oc base gen m =
  let fam = m.m_fam in
  Printf.ksprintf oc "fam ";
  print_parent oc base gen m.m_fath;
  Printf.ksprintf oc " +";
  print_date_option oc (Adef.od_of_cdate (get_marriage fam));
  let print_sexes s =
    let c x =
      match get_sex x with
      | Male -> 'm'
      | Female -> 'f'
      | Neuter -> '?'
    in
    Printf.ksprintf oc " %s %c%c" s (c m.m_fath) (c m.m_moth)
  in
  begin match get_relation fam with
    | Married -> ()
    | NotMarried -> Printf.ksprintf oc " #nm"
    | Engaged -> Printf.ksprintf oc " #eng"
    | NoSexesCheckNotMarried -> print_sexes "#nsck" ;
    | NoSexesCheckMarried -> print_sexes "#nsckm" ;
    | NoMention -> print_sexes "#noment"
    | MarriageBann -> print_sexes "#banns"
    | MarriageContract -> print_sexes "#contract"
    | MarriageLicense -> print_sexes "#license"
    | Pacs -> print_sexes "#pacs"
    | Residence -> print_sexes "#residence"
  end;
  print_if_no_empty oc base "#mp" (get_marriage_place fam);
  if Gwexport.(!opts.source) = None then
    print_if_no_empty oc base "#ms" (get_marriage_src fam);
  begin match get_divorce fam with
      NotDivorced -> ()
    | Separated -> Printf.ksprintf oc " #sep"
    | Divorced d ->
      let d = Adef.od_of_cdate d in
      Printf.ksprintf oc " -"; print_date_option oc d
  end;
  Printf.ksprintf oc " ";
  print_parent oc base gen m.m_moth;
  Printf.ksprintf oc "\n";
  Array.iter
    (fun ip ->
       if gen.per_sel ip then
         let p = poi base ip in
         Printf.ksprintf oc "wit";
         begin match get_sex p with
             Male -> Printf.ksprintf oc " m"
           | Female -> Printf.ksprintf oc " f"
           | _ -> ()
         end;
         Printf.ksprintf oc ": ";
         print_witness oc base gen p;
         Printf.ksprintf oc "\n")
    (get_witnesses fam);
  (match Gwexport.(!opts.source) with
   | None ->
     if sou base (get_fsources fam) <> ""
     then Printf.ksprintf oc "src %s\n" (correct_string base (get_fsources fam));
   | Some x -> Printf.ksprintf oc "src %s\n" (s_correct_string x) ) ;
  let csrc =
    match common_children_sources base m.m_chil with
      Some s -> Printf.ksprintf oc "csrc %s\n" (s_correct_string s); s
    | _ -> ""
  in
  let cbp =
    match common_children_birth_place base m.m_chil with
      Some s -> Printf.ksprintf oc "cbp %s\n" (s_correct_string s); s
    | _ -> ""
  in
  print_comment_for_family oc base gen fam;
  if not !old_gw && get_fevents fam <> [] then
    begin
      Printf.ksprintf oc "fevt\n";
      List.iter (print_fevent oc base gen false) (get_fevents fam);
      Printf.ksprintf oc "end fevt\n"
    end;
  begin match Array.length m.m_chil with
      0 -> ()
    | _ ->
      let fam_surname = get_surname m.m_fath in
      Printf.ksprintf oc "beg\n";
      Array.iter
        (fun p ->
           if gen.per_sel (get_iper p) then
             print_child oc base fam_surname csrc cbp p)
        m.m_chil;
      Printf.ksprintf oc "end\n"
  end;
  Gwdb.Marker.set gen.fam_done m.m_ifam true ;
  let f _ =
    Printf.sprintf "family \"%s.%d %s\" & \"%s.%d %s\""
      (p_first_name base m.m_fath) (get_new_occ m.m_fath)
      (p_surname base m.m_fath) (p_first_name base m.m_moth)
      (get_new_occ m.m_moth) (p_surname base m.m_moth)
  in
  let s =
    let sl =
      let acc =
        [ get_comment fam
        ; get_marriage_note fam
        ; get_marriage_src fam
        ]
      in
      if Gwexport.(!opts.source) = None then get_fsources fam :: acc else acc
    in
    let sl =
      if not !old_gw then
        let rec loop l accu =
          match l with
          | [] -> accu
          | evt :: l ->
            let acc =
              evt.efam_note
              :: (if Gwexport.(!opts.source) = None then evt.efam_src :: accu else accu)
            in
            loop l acc
        in
        loop (get_fevents fam) sl
      else sl
    in
    String.concat " " (List.map (sou base) sl)
  in
  ignore (add_linked_files gen f s [] : _ list)

let get_persons_with_notes m list =
  let list =
    let fath = m.m_fath in
    match get_parents fath with
      Some _ -> list
    | None -> fath :: list
  in
  let list =
    let moth = m.m_moth in
    match get_parents moth with
      Some _ -> list
    | None -> moth :: list
  in
  Array.fold_right List.cons m.m_chil list

let notes_aliases bdir =
  let fname = Filename.concat bdir "notes.alias" in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
    Some ic ->
    let rec loop list =
      match try Some (input_line ic) with End_of_file -> None with
        Some s ->
        let list =
          try
            let i = String.index s ' ' in
            (String.sub s 0 i,
             String.sub s (i + 1) (String.length s - i - 1)) ::
            list
          with Not_found -> list
        in
        loop list
      | None -> close_in ic; list
    in
    loop []
  | None -> []

let print_notes_for_person oc base gen p =
  let print_witness_in_notes witnesses =
    Array.iter
      (fun (ip, wk) ->
         let p = poi base ip in
         Printf.ksprintf oc "wit";
         begin match get_sex p with
             Male -> Printf.ksprintf oc " m"
           | Female -> Printf.ksprintf oc " f"
           | _ -> ()
         end;
         Printf.ksprintf oc ": ";
         begin match wk with
             Witness_GodParent -> Printf.ksprintf oc "#godp "
           | _ -> ()
         end;
         print_witness oc base gen p;
         Printf.ksprintf oc "\n")
      witnesses
  in
  let notes = sou base (get_notes p) in
  let surn = s_correct_string (p_surname base p) in
  let fnam = s_correct_string (p_first_name base p) in
  (* Si on n'est en mode old_gw, on mets tous les évènements dans les notes. *)
  if (notes <> "" || put_events_in_notes base p) && surn <> "?" &&
     fnam <> "?"
  then
    begin
      Printf.ksprintf oc "\n";
      Printf.ksprintf oc "notes %s %s%s\n" surn fnam
        (if get_new_occ p = 0 then ""
         else "." ^ string_of_int (get_new_occ p));
      Printf.ksprintf oc "beg\n";
      if notes <> "" then Printf.ksprintf oc "%s\n" notes;
      if put_events_in_notes base p then
        begin let rec loop pevents =
                match pevents with
                  [] -> ()
                | evt :: events ->
                  match evt.epers_name with
                    Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial |
                    Epers_Cremation ->
                    let name =
                      match evt.epers_name with
                        Epers_Birth -> "birth"
                      | Epers_Baptism -> "baptism"
                      | Epers_Death -> "death"
                      | Epers_Burial -> "burial"
                      | Epers_Cremation -> "creamation"
                      | _ -> ""
                    in
                    let notes = sou base evt.epers_note in
                    if notes <> "" then Printf.ksprintf oc "%s: %s\n" name notes;
                    print_witness_in_notes evt.epers_witnesses;
                    loop events
                  | _ -> print_pevent oc base gen evt; loop events
          in
          loop (get_pevents p)
        end;
      Printf.ksprintf oc "end notes\n"
    end;
  let f _ =
    Printf.sprintf "person \"%s.%d %s\"" (p_first_name base p) (get_new_occ p)
      (p_surname base p)
  in
  let s =
    let sl =
      [get_notes p; get_occupation p; get_birth_note p; get_birth_src p;
       get_baptism_note p; get_baptism_src p; get_death_note p;
       get_death_src p; get_burial_note p; get_burial_src p;
       get_psources p]
    in
    let sl =
      if not !old_gw then
        let rec loop l accu =
          match l with
            [] -> accu
          | evt :: l -> loop l (evt.epers_note :: evt.epers_src :: accu)
        in
        loop (get_pevents p) sl
      else sl
    in
    String.concat " " (List.map (sou base) sl)
  in
  ignore (add_linked_files gen f s [] : _ list)

let print_notes oc base gen ml =
  let pl =
    List.fold_right get_persons_with_notes ml gen.notes_pl_p
  in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else p :: pl) pl []
  in
  List.iter
    (fun p ->
       if gen.per_sel (get_iper p) then
         print_notes_for_person oc base gen p)
    pl

let is_isolated p =
  match get_parents p with
    Some _ -> false
  | None -> Array.length (get_family p) = 0

let is_definition_for_parent p =
  match get_parents p with
    Some _ -> false
  | None -> true

let get_isolated_related base m list =
  let concat_isolated p_relation ip list =
    let p = poi base ip in
    if List.mem_assq p list then list
    else if is_isolated p then
      match get_rparents p with
        {r_fath = Some x} :: _ when x = get_iper p_relation ->
        list @ [p, true]
      | {r_fath = None; r_moth = Some x} :: _
        when x = get_iper p_relation ->
        list @ [p, true]
      | _ -> list
    else list
  in
  let list =
    if is_definition_for_parent m.m_fath then
      List.fold_right (concat_isolated m.m_fath) (get_related m.m_fath)
        list
    else list
  in
  let list =
    if is_definition_for_parent m.m_moth then
      List.fold_right (concat_isolated m.m_moth) (get_related m.m_moth)
        list
    else list
  in
  Array.fold_right
    (fun p list ->
       List.fold_right (concat_isolated p) (get_related p) list)
    m.m_chil list

let get_persons_with_relations base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match get_rparents fath, get_parents fath with
      [], _ | _, Some _ -> list
    | _ -> (fath, false) :: list
  in
  let list =
    match get_rparents moth, get_parents moth with
      [], _ | _, Some _ -> list
    | _ -> (moth, false) :: list
  in
  let list =
    Array.fold_right
      (fun ip list ->
         let p = poi base ip in
         match get_rparents p, get_parents p with
           [], _ | _, Some _ -> list
         | {r_fath = Some x} :: _, _ when x <> get_iper m.m_fath ->
           list
         | _ -> (p, false) :: list)
      (get_witnesses m.m_fam) list
  in
  Array.fold_right
    (fun p list ->
       match get_rparents p with
         [] -> list
       | _ -> (p, false) :: list)
    m.m_chil list

let print_relation_parent oc base mark defined_p p =
  Printf.ksprintf oc "%s %s%s" (correct_string base (get_surname p))
    (correct_string base (get_first_name p))
    (if get_new_occ p = 0 then ""
     else "." ^ string_of_int (get_new_occ p));
  if Array.length (get_family p) = 0 && get_parents p = None &&
     not (Gwdb.Marker.get mark (get_iper p))
  then
    begin
      Gwdb.Marker.set mark (get_iper p) true ;
      if has_infos base p then print_infos oc base false "" "" p
      else Printf.ksprintf oc " 0";
      defined_p := p :: !defined_p
    end

let print_relation_for_person oc base gen def_p r =
  let fath =
    match r.r_fath with
      Some ip ->
      if gen.per_sel ip then
        let p = poi base ip in
        if sou base (get_first_name p) = "?" ||
           sou base (get_surname p) = "?"
        then
          None
        else Some p
      else None
    | None -> None
  in
  let moth =
    match r.r_moth with
      Some ip ->
      if gen.per_sel ip then
        let p = poi base ip in
        if sou base (get_first_name p) = "?" ||
           sou base (get_surname p) = "?"
        then
          None
        else Some p
      else None
    | None -> None
  in
  let err_same_sex =
    match fath, moth with
      Some fath, Some moth -> get_sex fath = get_sex moth
    | _ -> false
  in
  let print_err_one_relation p =
    Printf.ksprintf oc "- ";
    begin match r.r_type with
        Adoption -> Printf.ksprintf oc "adop"
      | Recognition -> Printf.ksprintf oc "reco"
      | CandidateParent -> Printf.ksprintf oc "cand"
      | GodParent -> Printf.ksprintf oc "godp"
      | FosterParent -> Printf.ksprintf oc "fost"
    end;
    if get_sex p = Male then Printf.ksprintf oc " fath" else Printf.ksprintf oc " moth";
    Printf.ksprintf oc ": ";
    print_relation_parent oc base gen.mark def_p p;
    Printf.ksprintf oc "\n"
  in
  match fath, moth with
    None, None -> ()
  | _ ->
    if err_same_sex then
      match fath, moth with
        Some fath, Some moth ->
        print_err_one_relation fath; print_err_one_relation moth
      | _ -> ()
    else
      begin
        Printf.ksprintf oc "- ";
        begin match r.r_type with
            Adoption -> Printf.ksprintf oc "adop"
          | Recognition -> Printf.ksprintf oc "reco"
          | CandidateParent -> Printf.ksprintf oc "cand"
          | GodParent -> Printf.ksprintf oc "godp"
          | FosterParent -> Printf.ksprintf oc "fost"
        end;
        begin match fath, moth with
            Some fath, None ->
            if get_sex fath = Male then Printf.ksprintf oc " fath"
            else Printf.ksprintf oc " moth"
          | None, Some moth ->
            if get_sex moth = Female then Printf.ksprintf oc " moth"
            else Printf.ksprintf oc " fath"
          | _ -> ()
        end;
        Printf.ksprintf oc ": ";
        begin match fath, moth with
            Some fath, None ->
            print_relation_parent oc base gen.mark def_p fath
          | None, Some moth ->
            print_relation_parent oc base gen.mark def_p moth
          | Some fath, Some moth ->
            if get_sex fath = Male && get_sex moth = Female then
              begin
                print_relation_parent oc base gen.mark def_p fath;
                Printf.ksprintf oc " + ";
                print_relation_parent oc base gen.mark def_p moth
              end
            else
              begin
                print_relation_parent oc base gen.mark def_p moth;
                Printf.ksprintf oc " + ";
                print_relation_parent oc base gen.mark def_p fath
              end
          | _ -> ()
        end;
        Printf.ksprintf oc "\n"
      end

let print_relations_for_person oc base gen def_p is_definition p =
  let surn = correct_string base (get_surname p) in
  let fnam = correct_string base (get_first_name p) in
  let exist_relation =
    List.exists
      (fun r ->
         match r.r_fath, r.r_moth with
           Some ip1, Some ip2 -> gen.per_sel ip1 && gen.per_sel ip2
         | Some ip1, _ -> gen.per_sel ip1
         | _, Some ip2 -> gen.per_sel ip2
         | _ -> false)
      (get_rparents p)
  in
  if surn <> "?" && fnam <> "?" && exist_relation &&
     not (Gwdb.Marker.get gen.mark_rel (get_iper p))
  then
    begin
      Gwdb.Marker.set gen.mark_rel (get_iper p) true;
      Printf.ksprintf oc "\n";
      Printf.ksprintf oc "rel %s %s%s" surn fnam
        (if get_new_occ p = 0 then ""
         else "." ^ string_of_int (get_new_occ p));
      if is_definition then
        begin
          Gwdb.Marker.set gen.mark (get_iper p) true;
          def_p := p :: !def_p;
          if has_infos base p then print_infos oc base false "" "" p
          else Printf.ksprintf oc " 0";
          match get_sex p with
            Male -> Printf.ksprintf oc " #h"
          | Female -> Printf.ksprintf oc " #f"
          | Neuter -> ()
        end;
      Printf.ksprintf oc "\n";
      Printf.ksprintf oc "beg\n";
      List.iter (print_relation_for_person oc base gen def_p)
        (get_rparents p);
      Printf.ksprintf oc "end\n"
    end

let print_relations oc base gen ml =
  let pl = List.fold_right (get_persons_with_relations base) ml [] in
  let pl = List.fold_right (get_isolated_related base) ml pl in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key_fst p pl then pl else p :: pl) pl
      []
  in
  let rec loop =
    function
      [] -> ()
    | (p, if_def) :: pl ->
      let def_p = ref [] in
      if get_rparents p <> [] && gen.per_sel (get_iper p) then
        begin
          print_relations_for_person oc base gen def_p if_def p;
          List.iter (print_notes_for_person oc base gen) !def_p;
          if not !old_gw then
            List.iter (print_pevents_for_person oc base gen) !def_p
        end;
      loop (pl @ List.map (fun p -> p, false) !def_p)
  in
  loop pl

let print_isolated_relations oc base gen p =
  let pl = [p, false] in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key_fst p pl then pl else p :: pl) pl
      []
  in
  let rec loop =
    function
      [] -> ()
    | (p, if_def) :: pl ->
      let def_p = ref [] in
      if get_rparents p <> [] && gen.per_sel (get_iper p) then
        begin
          print_relations_for_person oc base gen def_p if_def p;
          List.iter (print_notes_for_person oc base gen) !def_p
        end;
      loop (pl @ List.map (fun p -> p, false) !def_p)
  in
  loop pl

let rec merge_families ifaml1f ifaml2f =
  match ifaml1f, ifaml2f with
    ifam1 :: ifaml1, ifam2 :: ifaml2 ->
    let m1 = List.mem ifam1 ifaml2 in
    let m2 = List.mem ifam2 ifaml1 in
    if m1 && m2 then merge_families ifaml1 ifaml2
    else if m1 then ifam2 :: merge_families ifaml1f ifaml2
    else if m2 then ifam1 :: merge_families ifaml1 ifaml2f
    else if ifam2 < ifam1 then
      ifam2 :: ifam1 :: merge_families ifaml1 ifaml2
    else if ifam1 < ifam2 then
      ifam1 :: ifam2 :: merge_families ifaml1 ifaml2
    else ifam1 :: merge_families ifaml1 ifaml2
  | ifaml1, [] -> ifaml1
  | [], ifaml2 -> ifaml2


(* let connected_families base fam_sel ifam cpl =
 *   let rec loop ifaml scanned =
 *     function
 *     | ip :: ipl ->
 *       let scanned = ip :: scanned in
 *       let ipl, ifaml =
 *         Array.fold_right begin fun i (acci, accf) ->
 *           if fam_sel i && not @@ List.mem i accf
 *           then
 *             let accf = i :: accf in
 *             let cpl = foi base ifam in
 *             let fa = get_father cpl in
 *             let mo = get_mother cpl in
 *             let acci =
 *               if not @@ List.mem fa acci
 *               && not @@ List.mem fa scanned
 *               then fa :: acci
 *               else acci
 *             in
 *             let acci =
 *               if not @@ List.mem mo acci
 *               && not @@ List.mem mo scanned
 *               then mo :: acci
 *               else acci
 *             in
 *             (acci, accf)
 *           else (acci, accf)
 *         end (get_family @@ poi base ip) (ipl, ifaml)
 *       in
 *       loop ifaml scanned ipl
 *     | [] -> ifaml
 *   in
 *   loop [ ifam ] [] [ get_father cpl ; get_mother cpl ]
 *   |> List.sort_uniq compare *)


let rec filter f =
  function
    x :: l -> if f x then x :: filter f l else filter f l
  | [] -> []

let connected_families base fam_sel ifam cpl =
  let rec loop ifaml ipl_scanned =
    function
      ip :: ipl ->
      if List.mem ip ipl_scanned then loop ifaml ipl_scanned ipl
      else
        let u = poi base ip in
        let ifaml1 = Array.to_list (get_family u) in
        let ifaml1 = filter fam_sel ifaml1 in
        let ifaml = merge_families ifaml ifaml1 in
        let ipl =
          List.fold_right
            (fun ifam ipl ->
               let cpl = foi base ifam in
               get_father cpl :: get_mother cpl :: ipl)
            ifaml1 ipl
        in
        loop ifaml (ip :: ipl_scanned) ipl
    | [] -> ifaml
  in
  loop [ifam] [] [get_father cpl]

let find_person base p1 po p2 =
  match person_of_key base p1 p2 po with
    Some ip -> ip
  | None ->
    Printf.printf "Not found: %s%s %s\n" p1
      (if po = 0 then "" else " " ^ string_of_int po) p2;
    flush stdout;
    exit 2

let read_file_contents fname =
  match try Some (open_in fname) with Sys_error _ -> None with
    Some ic ->
    let len = ref 0 in
    begin try
        let rec loop () =
          len := Buff.store !len (input_char ic); loop ()
        in
        loop ()
      with End_of_file -> Buff.get !len
    end
  | None -> ""

type separate = ToSeparate | NotScanned | BeingScanned | Scanned

let rec find_ancestors base surn p list =
  match get_parents p with
    Some ifam ->
    let cpl = foi base ifam in
    let fath = poi base (get_father cpl) in
    let moth = poi base (get_mother cpl) in
    if not (eq_istr (get_surname fath) surn) &&
       not (eq_istr (get_surname moth) surn)
    then
      p :: list
    else
      let list =
        if eq_istr (get_surname fath) surn then
          find_ancestors base surn fath list
        else list
      in
      if eq_istr (get_surname moth) surn then
        find_ancestors base surn moth list
      else list
  | None -> p :: list

let mark_branch base mark surn p =
  let rec loop top p =
    for i = 0 to Array.length (get_family p) - 1 do
      let ifam = (get_family p).(i) in
      if Gwdb.Marker.get mark ifam = NotScanned then begin
        let ifaml =
          connected_families base (fun _ -> true) ifam (foi base ifam)
        in
        let children =
          List.fold_left
            (fun list ifam ->
               let desc = foi base ifam in
               Array.fold_left (fun list ip -> poi base ip :: list) list
                 (get_children desc))
            [] ifaml
        in
        if top || List.exists (fun p -> eq_istr (get_surname p) surn) children
        then
          begin
            List.iter
              (fun ifam -> Gwdb.Marker.set mark ifam ToSeparate)
              ifaml;
            List.iter (loop false) children
          end
      end
    done
  in
  loop true p

let mark_someone base mark s =
  match Gutil.person_ht_find_all base s with
    [ip] ->
    let p = poi base ip in
    let plist = find_ancestors base (get_surname p) p [] in
    List.iter (mark_branch base mark (get_surname p)) plist
  | [] -> Printf.eprintf "Error: \"%s\" is not found\n" s; flush stderr; exit 2
  | _ ->
    Printf.eprintf "Error: several answers for \"%s\"\n" s;
    flush stderr;
    exit 2


let scan_connex_component base test_action len ifam =
  let rec loop len ifam =
    let fam = foi base ifam in
    let fath = poi base (get_father fam) in
    let moth = poi base (get_mother fam) in
    let len =
      Array.fold_left
        (fun len ifam1 ->
           if ifam1 = ifam then len else test_action loop len ifam1)
        len (get_family fath)
    in
    let len =
      Array.fold_left
        (fun len ifam1 ->
           if ifam1 = ifam then len else test_action loop len ifam1)
        len (get_family moth)
    in
    let len =
      match get_parents fath with
        Some ifam -> test_action loop len ifam
      | _ -> len
    in
    let len =
      match get_parents moth with
        Some ifam -> test_action loop len ifam
      | _ -> len
    in
    let children = get_children fam in
    Array.fold_left
      (fun len ip ->
         Array.fold_left (test_action loop) len
           (get_family (poi base ip)))
      len children
  in
  loop len ifam

let mark_one_connex_component base mark ifam =
  let origin_file = sou base (get_origin_file (foi base ifam)) in
  let test_action loop len ifam =
    if Gwdb.Marker.get mark ifam = NotScanned &&
       sou base (get_origin_file (foi base ifam)) = origin_file
    then
      begin
        Gwdb.Marker.set mark ifam BeingScanned;
        loop (len + 1) ifam
      end
    else len
  in
  let _ = test_action (fun _ _ -> 1) 0 ifam in
  let len = 1 + scan_connex_component base test_action 0 ifam in
  let set_mark x =
    let test_action loop () ifam =
      if Gwdb.Marker.get mark ifam = BeingScanned then
        begin Gwdb.Marker.set mark ifam x; loop () ifam end
    in
    test_action (fun _ _ -> ()) () ifam;
    scan_connex_component base test_action () ifam
  in
  if len <= !sep_limit && (!only_file = "" || !only_file = origin_file)
  then
    set_mark ToSeparate
  else
    begin
      Printf.eprintf "%s: group of size %d not included\n" origin_file len;
      let cpl = foi base ifam in
      Printf.eprintf "    %s + %s\n"
        (Gutil.designation base (poi base (get_father cpl)))
        (Gutil.designation base (poi base (get_mother cpl)));
      flush stderr;
      set_mark Scanned
    end

let mark_connex_components base mark ifam =
  let test_action _loop _len ifam =
    if Gwdb.Marker.get mark ifam = NotScanned then
      mark_one_connex_component base mark ifam
  in
  scan_connex_component base test_action () ifam

let add_small_connex_components base mark =
  Gwdb.Collection.iter
    (fun i ->
       if Gwdb.Marker.get mark i = ToSeparate
       then mark_connex_components base mark i)
    (Gwdb.ifams base)

let separate base =
  match List.rev !separate_list with
  | [] -> (fun _ -> false)
  | list ->
    let ifams = Gwdb.ifams base in
    let mark = Gwdb.ifam_marker (Gwdb.ifams base) NotScanned in
    List.iter (mark_someone base mark) list;
    add_small_connex_components base mark;
    let len =
      Gwdb.Collection.fold
        (fun acc i ->
           if Gwdb.Marker.get mark i = ToSeparate then (acc + 1) else acc)
        0 ifams
    in
    Printf.eprintf "*** extracted %d families\n" len;
    flush stderr;
    fun ifam -> Gwdb.Marker.get mark ifam = ToSeparate

let rs_printf oc s =
  let rec loop bol i =
    if i = String.length s then ()
    else if s.[i] = '\n' then begin Printf.ksprintf oc "\n"; loop true (i + 1) end
    else
      begin
        if bol then Printf.ksprintf oc "  ";
        Printf.ksprintf oc "%c" s.[i];
        loop false (i + 1)
      end
  in
  loop true 0

let gwu opts base in_dir out_dir src_oc_ht (per_sel, fam_sel) =
  let to_separate = separate base in
  let out_oc_first = ref true in
  let _ofile, oc, close = opts.Gwexport.oc in
  let origin_file fname =
    if out_dir = "" then oc, out_oc_first, close
    else if fname = "" then oc, out_oc_first, close
    else
      try Hashtbl.find src_oc_ht fname with
        Not_found ->
        let oc = open_out (Filename.concat out_dir fname) in
        let out, _, _ as x = output_string oc, ref true, fun () -> close_out oc in
        if not !raw_output then out "encoding: utf-8\n";
        if !old_gw then out "\n" else out "gwplus\n\n";
        Hashtbl.add src_oc_ht fname x;
        x
  in
  let gen =
    let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
    let mark_rel = Gwdb.iper_marker (Gwdb.ipers base) false in
    let fam_done = Gwdb.ifam_marker (Gwdb.ifams base) false in
    {mark = mark; mark_rel = mark_rel; per_sel = per_sel;
     fam_sel = fam_sel; fam_done = fam_done; notes_pl_p = [];
     ext_files = []; notes_alias = notes_aliases in_dir;
     pevents_pl_p = []}
  in
  let nb_fam = nb_of_families base in
  if !(Mutil.verbose) then ProgrBar.start ();
  Gwdb.Collection.iteri begin fun i ifam ->
    if !(Mutil.verbose) then ProgrBar.run i nb_fam;
    if not (Gwdb.Marker.get gen.fam_done ifam) then
      let fam = foi base ifam in
      let ifaml = connected_families base gen.fam_sel ifam fam in
      let (oc, first, _close) =
        if to_separate ifam then oc, out_oc_first, close
        else origin_file (sou base (get_origin_file fam))
      in
      let ml =
        List.fold_right
          (fun ifam ml ->
             let fam = foi base ifam in
             let m =
               {m_ifam = ifam; m_fam = fam;
                m_fath = poi base (get_father fam);
                m_moth = poi base (get_mother fam);
                m_chil = Array.map (fun ip -> poi base ip) (get_children fam)}
             in
             if empty_family base m then
               begin
                 Gwdb.Marker.set gen.fam_done m.m_ifam true ;
                 ml
               end
             else m :: ml)
          ifaml []
      in
      if ml <> [] then
        begin
          gen.notes_pl_p <- [];
          gen.pevents_pl_p <- [];
          if not !first then Printf.ksprintf oc "\n";
          first := false;
          List.iter (print_family oc base gen) ml;
          print_notes oc base gen ml;
          print_relations oc base gen ml;
          if not !old_gw then print_pevents oc base gen ml
        end
  end (Gwdb.ifams ~select:gen.fam_sel base) ;
  (* Ajout des personnes isolée à l'export. On leur ajoute des    *)
  (* parents pour pouvoir utiliser les autres fonctions normales. *)
  (* Export que si c'est toute la base.                           *)
  if !isolated
  && opts.Gwexport.asc = None
  && opts.Gwexport.desc = None
  && opts.Gwexport.ascdesc = None
  then
    Gwdb.Collection.iter begin fun i ->
      if Gwdb.Marker.get gen.mark i || Gwdb.Marker.get gen.mark_rel i then ()
      else
        let p = poi base i in
        match get_parents p with
          Some _ -> ()
        | None ->
          if bogus_person base p &&
             not
               (get_birth p <> Adef.cdate_None ||
                get_baptism p <> Adef.cdate_None ||
                get_first_names_aliases p <> [] ||
                get_surnames_aliases p <> [] ||
                sou base (get_public_name p) <> "" ||
                get_qualifiers p <> [] || get_aliases p <> [] ||
                get_titles p <> [] ||
                sou base (get_occupation p) <> "" ||
                sou base (get_birth_place p) <> "" ||
                sou base (get_birth_src p) <> "" ||
                sou base (get_baptism_place p) <> "" ||
                sou base (get_baptism_src p) <> "" ||
                sou base (get_death_place p) <> "" ||
                sou base (get_death_src p) <> "" ||
                sou base (get_burial_place p) <> "" ||
                sou base (get_burial_src p) <> "" ||
                sou base (get_notes p) <> "" ||
                sou base (get_psources p) <> "" ||
                get_rparents p <> [] || get_related p <> [])
          then
            ()
          else
            let (oc, _first, _) =
              origin_file (base_notes_origin_file base)
            in
            Printf.ksprintf oc "\n";
            print_empty_family oc base p;
            print_notes_for_person oc base gen p;
            Gwdb.Marker.set gen.mark i true;
            print_isolated_relations oc base gen p
    end (Gwdb.ipers base) ;
  if !(Mutil.verbose) then ProgrBar.finish ();
  if not Gwexport.(!opts.no_notes) then
    let s = base_notes_read base "" in
    let (oc, first, _) = origin_file (base_notes_origin_file base) in
    if s <> "" then
      begin
        if not !first then Printf.ksprintf oc "\n";
        first := false;
        Printf.ksprintf oc "notes-db\n";
        rs_printf oc s;
        Printf.ksprintf oc "\nend notes-db\n";
        ignore
          (add_linked_files gen (fun _ -> "database notes") s [] : _ list)
      end;
    begin try
        let files =
          Sys.readdir (Filename.concat in_dir (base_wiznotes_dir base))
        in
        Array.sort compare files;
        for i = 0 to Array.length files - 1 do
          let file = files.(i) in
          if Filename.check_suffix file ".txt" then
            let wfile =
              List.fold_left Filename.concat in_dir
                [base_wiznotes_dir base; file]
            in
            let s = read_file_contents wfile in
            ignore
              (add_linked_files gen (fun _ -> "wizard \"" ^ file ^ "\"") s
                 [] :
                 _ list)
        done
      with Sys_error _ -> ()
    end;
    let rec loop =
      function
        [] -> ()
      | (f, _) :: files ->
        let fn =
          match NotesLinks.check_file_name f with
            Some (dl, f) -> List.fold_right Filename.concat dl f
          | None -> "bad"
        in
        let s = base_notes_read base fn in
        let files =
          add_linked_files gen
            (fun _ -> Printf.sprintf "extended page \"%s\"" f) s files
        in
        loop files
    in
    loop gen.ext_files;
    List.iter
      (fun (f, r) ->
         let fn =
           match NotesLinks.check_file_name f with
             Some (dl, f) -> List.fold_right Filename.concat dl f
           | None -> "bad"
         in
         let s = String.trim (base_notes_read base fn) in
         if s <> "" then
           begin
             if not !first then Printf.ksprintf oc "\n";
             first := false;
             Printf.ksprintf oc "# extended page \"%s\" used by:\n" f;
             List.iter (fun f -> Printf.ksprintf oc "#  - %s\n" f)
               (List.sort compare !r);
             Printf.ksprintf oc "page-ext %s\n" f;
             rs_printf oc s;
             Printf.ksprintf oc "\nend page-ext\n"
           end)
      (List.sort compare gen.ext_files);
    let close () = flush_all () ; close () ; Hashtbl.iter (fun _ (_, _, close) -> close ()) src_oc_ht in
    try
      let files =
        Sys.readdir (Filename.concat in_dir (base_wiznotes_dir base))
      in
      Array.sort compare files;
      for i = 0 to Array.length files - 1 do
        let file = files.(i) in
        if Filename.check_suffix file ".txt" then
          let wizid = Filename.chop_suffix file ".txt" in
          let wfile =
            List.fold_left Filename.concat in_dir
              [base_wiznotes_dir base; file]
          in
          let s = String.trim (read_file_contents wfile) in
          Printf.ksprintf oc "\nwizard-note %s\n" wizid;
          rs_printf oc s;
          Printf.ksprintf oc "\nend wizard-note\n"
      done ;
      close ()
    with Sys_error _ -> close ()
