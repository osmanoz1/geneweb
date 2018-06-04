(* $Id: public2.ml,v 4.1 2008/03/31 11:34:34 deraugla Exp $ *)

open Printf

open Def
open Gwdb

let year_of p =
  match
    Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
    get_death p, CheckItem.date_of_death (get_death p)
  with
    _, _, NotDead, _ -> None
  | Some (Dgreg (d, _)), _, _, _ -> Some d.year
  | _, Some (Dgreg (d, _)), _, _ -> Some d.year
  | _, _, _, Some (Dgreg (d, _)) -> Some d.year
  | _ -> None

let find_dated_ancestor base p =
  let mark = Array.make (nb_of_persons base) false in
  let rec loop nb_gen iplist =
    if iplist = [] then None
    else
      let anc_list =
        List.fold_left
          (fun anc_list ip ->
             match get_parents (poi base ip) with
               Some ifam ->
                 let fam = foi base ifam in
                 get_mother fam :: get_father fam :: anc_list
             | None -> anc_list)
          [] iplist
      in
      (* Dans le cas où le nombre d'implexes est très élevé, le calcul *)
      (* peut être très long car on le refait plusieurs fois pour les  *)
      (* mêmes personnes. On rend donc la liste unique.                *)
      let anc_list = Mutil.list_uniq anc_list in
      let anc_list =
        List.filter (fun ip -> not mark.(Adef.int_of_iper ip)) anc_list
      in
      let () =
        List.iter (fun ip -> mark.(Adef.int_of_iper ip) <- true) anc_list
      in
      let rec loop_ind =
        function
          ip :: iplist ->
            let p = poi base ip in
            begin match year_of p with
              Some year -> Some (p, year, nb_gen)
            | None -> loop_ind iplist
            end
        | [] -> loop (nb_gen + 1) anc_list
      in
      loop_ind anc_list
  in
  loop 1 [get_key_index p]

let nb_years_by_gen = 30

let change_somebody_access base lim_year trace p year_of_p =
  if year_of_p = None && get_access p = IfTitles then
    match find_dated_ancestor base p with
      Some (a, year, nb_gen) ->
        let acc =
          if year + nb_gen * nb_years_by_gen > lim_year then IfTitles
          else Public
        in
        let gp = {(gen_person_of_person p) with access = acc} in
        patch_person base gp.key_index gp;
        if trace && acc <> IfTitles then
          begin
            printf "%s -> " (Gutil.designation base p);
            if acc = Private then printf "private" else printf "public";
            printf " (anc %d gen %s year %d)" nb_gen
              (Gutil.designation base a) year;
            printf "\n";
            flush stdout;
            Some acc
          end
        else None
    | None -> None
  else None

let public_all bname lim_year trace =
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  Consang.check_noloop base
    (function
       OwnAncestor p ->
         printf "I cannot deal this database.\n";
         printf "%s is his own ancestors\n" (Gutil.designation base p);
         flush stdout;
         exit 2
     | _ -> assert false);
  let n = nb_of_persons base in
  let changes = ref false in
  ProgrBar.start ();
  for i = 0 to n - 1 do
    ProgrBar.run i n;
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    if year_of p = None && get_access p = IfTitles then
      match change_somebody_access base lim_year trace p (year_of p) with
        Some _ -> changes := true
      | None ->
          let fama = get_family p in
          let rec loop i =
            if i = Array.length fama then ()
            else
              let ifam = fama.(i) in
              let isp = Gutil.spouse ip (foi base ifam) in
              let sp = poi base isp in
              let year_of_sp = year_of sp in
              let acc_opt =
                match year_of_sp with
                  Some year ->
                    Some (if year > lim_year then IfTitles else Public)
                | None ->
                    change_somebody_access base lim_year trace sp year_of_sp
              in
              match acc_opt with
                Some acc ->
                  let gp = {(gen_person_of_person p) with access = acc} in
                  patch_person base gp.key_index gp;
                  changes := true;
                  if trace && acc <> IfTitles then
                    begin
                      printf "%s -> " (Gutil.designation base p);
                      if acc = Private then printf "private"
                      else printf "public";
                      printf " (inherited from spouse %s)"
                        (Gutil.designation base sp);
                      printf "\n";
                      flush stdout
                    end
              | None -> loop (i + 1)
          in
          loop 0
  done;
  if !changes then commit_patches base;
  ProgrBar.finish ()

let lim_year = ref 1900
let trace = ref false
let bname = ref ""

let speclist =
  ["-y", Arg.Int (fun i -> lim_year := i),
   "limit year (default = " ^ string_of_int !lim_year ^ ")";
   "-t", Arg.Set trace, "trace changed persons"]
let anonfun i = bname := i
let usage = "Usage: public [-y #] [-t] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  match
    Lock.control (Mutil.lock_file !bname) false
      (fun () -> public_all !bname !lim_year !trace)
  with
    Some x -> x
  | None ->
      eprintf "Base is locked. Waiting... ";
      flush stderr;
      match
        Lock.control (Mutil.lock_file !bname) true
          (fun () ->
             eprintf "Ok\n"; flush stderr; public_all !bname !lim_year !trace)
      with
        Some x -> x
      | None ->
          printf "\nSorry. Impossible to lock base.\n"; flush stdout; exit 2

let _ = main ()
