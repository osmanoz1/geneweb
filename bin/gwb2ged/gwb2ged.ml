(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Gwb2gedlib

let ifile = ref ""
let ofile = ref "a.ged"
let mem = ref false
let anc_1st = ref ""
let anc_occ = ref 0
let anc_2nd = ref ""
let desc_1st = ref ""
let desc_occ = ref 0
let desc_2nd = ref ""
type arg_state =
  ASnone | ASwaitAncOcc | ASwaitAncSurn | ASwaitDescOcc | ASwaitDescSurn
let arg_state = ref ASnone

let errmsg =
  "Usage: "
  ^ Sys.argv.(0)
  ^ " <base> [options]\n\
     If both options -a and -d are used, intersection is assumed.\n\
     If several options -s are used, union is assumed.\n\
     Options are:"

let speclist =
  ["-charset",
   Arg.String
     (fun x ->
        arg_state := ASnone;
        match x with
          "ASCII" -> charset := Ascii
        | "ANSEL" -> charset := Ansel
        | "ANSI" -> charset := Ansi
        | "UTF-8" -> charset := Utf8
        | _ -> raise (Arg.Bad "bad -charset value")),
   "[ASCII|ANSEL|ANSI|UTF-8]: set charset; default is UTF-8.";
   "-o", Arg.String (fun x -> ofile := x; arg_state := ASnone),
   "<ged>: output file name (default: a.ged)";
   "-mem", Arg.Unit (fun () -> mem := true; arg_state := ASnone),
   ": save memory space, but slower";
   "-a", Arg.String (fun s -> anc_1st := s; arg_state := ASwaitAncOcc),
   "\"<1st_name>\" [num] \"<surname>\": select ancestors of";
   "-d", Arg.String (fun s -> desc_1st := s; arg_state := ASwaitDescOcc),
   "\"<1st_name>\" [num] \"<surname>\": select descendants of";
   "-aws",
   Arg.String
     (fun s ->
        anc_1st := s; arg_state := ASwaitAncOcc; with_siblings := true),
   "\"<1st_name>\" [num] \"<surname>\" : select ancestors with siblings";
   "-s", Arg.String (fun x -> surnames := x :: !surnames),
   "\"<surname>\" : select this surname (option usable several times)";
   "-nsp", Arg.Set no_spouses_parents,
   ": no spouses' parents (for options -s and -d)";
   "-nn", Arg.Set no_notes, ": no (database) notes";
   "-nopicture", Arg.Set no_picture, ": Don't extract individual picture.";
   "-picture-path", Arg.Set picture_path, ": Extract pictures path.";
   "-indexes", Arg.Set with_indexes, ": Export indexes in gedcom.";
   "-c", Arg.Int (fun i -> censor := i),
   "<num> :\n     \
    When a person is born less than <num> years ago, it is not exported \
    unless it is Public. All the spouses and descendants are also censored."]

let anonfun s =
  match !arg_state with
    ASnone ->
      if !ifile = "" then ifile := s
      else raise (Arg.Bad "Cannot treat several databases")
  | ASwaitAncOcc ->
      begin try anc_occ := int_of_string s; arg_state := ASwaitAncSurn with
        Failure _ -> anc_occ := 0; anc_2nd := s; arg_state := ASnone
      end
  | ASwaitAncSurn -> anc_2nd := s; arg_state := ASnone
  | ASwaitDescOcc ->
      begin try desc_occ := int_of_string s; arg_state := ASwaitDescSurn with
        Failure _ -> desc_occ := 0; desc_2nd := s; arg_state := ASnone
      end
  | ASwaitDescSurn -> desc_2nd := s; arg_state := ASnone

let main () =
  Arg.parse speclist anonfun errmsg;
  Secure.set_base_dir (Filename.dirname !ifile);
  let anc =
    if !anc_1st <> "" then
      if !anc_2nd = "" then
        begin
          Printf.printf "Misused option -a\n";
          Printf.printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        end
      else Some (!anc_1st, !anc_occ, !anc_2nd)
    else None
  in
  let desc =
    if !desc_1st <> "" then
      if !desc_2nd = "" then
        begin
          Printf.printf "Misused option -d\n";
          Printf.printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        end
      else Some (!desc_1st, !desc_occ, !desc_2nd)
    else None
  in
  if !ofile = "-" then ofile := "";
  if !ifile = "" then
    begin
      Printf.printf "Missing base name\n";
      Printf.printf "Use option -help for usage\n";
      flush stdout;
      exit 2
    end;
  match try Some (Gwdb.open_base !ifile) with Sys_error _ -> None with
  | Some base ->
    nb_persons := Gwdb.nb_of_persons base ;
    nb_families := Gwdb.nb_of_families base ;
    gwb2ged base !ifile !ofile anc desc !mem
  | None -> Printf.printf "Can't open base %s\n" !ifile; flush stdout; exit 2

let _ = Printexc.catch main ()
