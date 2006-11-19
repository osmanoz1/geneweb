(* camlp4r ./pa_html.cmo *)
(* $Id: wiznotes.ml,v 5.35 2006-11-19 13:56:33 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Util;
open Def;

value dir conf base =
  Filename.concat (Util.base_path [] (conf.bname ^ ".gwb"))
    (Gwdb.base_wiznotes_dir base)
;

value wzfile wddir wz = Filename.concat wddir (wz ^ ".txt");

value read_auth_file fname =
  let data = read_gen_auth_file fname in
  List.map
    (fun au ->
       let wizname =
         try
           let k = String.index au.au_info ':' in
           String.sub au.au_info 0 k
         with
         [ Not_found -> "" ]
       in
       let (wizname, wizorder, islash) =
         try
           let i = String.index wizname '/' in
           let w1 = String.sub wizname 0 i in
           let l = String.length wizname in
           let w2 = String.sub wizname (i + 1) (l - i - 1) in
           (w1 ^ w2, w2 ^ w1, i)
         with
         [ Not_found -> (wizname, "~", -1) ]
       in
       (au.au_user, (wizname, (wizorder, islash))))
    data
;

value read_wizard_notes fname =
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let (date, len) =
        try
          let line = input_line ic in
          if line = "WIZNOTES" then
            let line = input_line ic in
            (float_of_string line, 0)
          else
            let s = Unix.stat fname in
            (s.Unix.st_mtime, Buff.store (Buff.mstore 0 line) '\n')
        with
        [ End_of_file | Failure _ -> (0., 0) ]
      in
      let rec loop len =
        match try Some (input_char ic) with [ End_of_file -> None ] with
        [ Some c -> loop (Buff.store len c)
        | None -> do { close_in ic; len } ]
      in
      let len = loop len in
      (Buff.get len, date)
  | None -> ("", 0.) ]
;

value write_wizard_notes fname nn =
  if nn = "" then try Sys.remove fname with [ Sys_error _ -> () ]
  else
    match try Some (Secure.open_out fname) with [ Sys_error _ -> None ] with
    [ Some oc ->
        do {
          Printf.fprintf oc "WIZNOTES\n%.0f\n" (Unix.time ());
          output_string oc nn;
          output_string oc "\n";
          close_out oc
        }
    | None -> () ]
;

value wiznote_date wfile =
  match try Some (Secure.open_in wfile) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let date =
        try
          let line = input_line ic in
          if line = "WIZNOTES" then float_of_string (input_line ic)
          else raise Exit
        with
        [ End_of_file | Failure _ | Exit ->
            let s = Unix.stat wfile in
            s.Unix.st_mtime ]
      in
      do { close_in ic; (wfile, date) }
  | None -> ("", 0.) ]
;

type item 'a =
  [ ItemChar of string
  | ItemSpace of int
  | ItemWiz of 'a ]
;

value dispatch_in_columns ncol list =
  let (rlist, len, _) =
    List.fold_left
      (fun (rlist, len, prev) wiz ->
         let (_, (_, (ord, _)), _, _) = wiz in
         let (rlist, len) =
           match prev with
           [ Some prev_ord when ord.[0] = prev_ord.[0] -> (rlist, len)
           | _ ->
               ([ItemSpace 1; ItemSpace 2; ItemChar ord :: rlist],
                len + 3) ]
         in
         let rlist = [ItemWiz wiz :: rlist] in
         (rlist, len + 1, Some ord))
      ([], 0, None) list
  in
  let list = List.rev rlist in
  loop [] len 0 ncol where rec loop len_list len cum_len ncol =
    if ncol = 1 then (List.rev [len :: len_list], list)
    else
      let len_i =
        let len_i = (len + ncol / 2) / ncol in
        match List.nth list (cum_len + len_i - 1) with
        [ ItemChar c -> len_i + 3
        | ItemSpace n -> len_i + n
        | ItemWiz (wz, _, _, _) -> len_i ]
      in
      loop [len_i :: len_list] (len - len_i) (cum_len + len_i) (ncol - 1)
;

value print_wizards_by_alphabetic_order conf list = do {
  let (len_list, list) =
    let ncols =
      match p_getint conf.env "ncols" with
      [ Some n -> max 1 n
      | None -> if List.length list < 10 then 1 else 3 ]
    in
    dispatch_in_columns ncols list
  in
  Util.begin_centered conf;
  tag "table" "width=\"95%%\" border=\"%d\"" conf.border begin
    tag "tr" "valign=\"top\"" begin
      let _ =
        List.fold_left
          (fun (list, prev_char) len ->
             loop prev_char True len list
             where rec loop prev_char top n list =
               if n = 0 then do {
                 Wserver.wprint "</ul>\n</td>\n";
                 (list, prev_char)
               }
               else do {
                 if top then Wserver.wprint "<td>\n" else ();
                 match list with
                 [ [item :: list] -> do {
                     match item with
                     [ ItemChar c -> do {
                         if not top then Wserver.wprint "</ul>\n" else ();
                         Wserver.wprint "<h3 style=\"border-bottom: \
                           dotted 1px\">%c</h3>\n" c.[0];
                         Wserver.wprint "<ul>\n";
                       }
                     | ItemSpace _ -> ()
                     | ItemWiz (wz, (wname, (_, islash)), wfile, stm) -> do {
                         let tm = Unix.localtime stm in
                         if top then do {
                           Wserver.wprint "<h3 style=\"border-bottom: \
                             dotted 1px\">%c (%s)</h3>\n"
                             prev_char.[0] (transl conf "continued");
                           Wserver.wprint "<ul>\n";
                         }
                         else ();
                         let with_link =
                           conf.wizard && conf.user = wz || wfile <> "" ||
                           conf.manitou
                         in
                         stag "li" begin
                           if with_link then
                             Wserver.wprint
                               "<a href=\"%sm=WIZNOTES;f=%s%t\">"
                               (commd conf) (Util.code_varenv wz)
                               (fun _ ->
                                  Wserver.wprint
                                   ";d=%d-%02d-%02d,%02d:%02d:%02d"
                                   (tm.Unix.tm_year + 1900)
                                   (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
                                   tm.Unix.tm_hour tm.Unix.tm_min
                                   tm.Unix.tm_sec)
                           else ();
                           if islash > 0 then
                             let s1 =
                               let islash =
                                 if wname.[islash-1] = ' ' then
                                   islash - 1
                                 else islash
                               in
                               String.sub wname 0 islash
                             in
                             let s2 =
                               String.sub wname islash
                                 (String.length wname - islash)
                             in
                             Wserver.wprint "%s (%s)" s2 s1
                           else
                             Wserver.wprint "%s" wname;
                           if with_link then Wserver.wprint "</a>"
                           else ();
                         end;
                         Wserver.wprint "\n";
                       } ];
                     let prev_char =
                       match item with
                       [ ItemChar c -> c
                       | _ -> prev_char ]
                     in
                     loop prev_char False (n - 1) list
                   }
                 | [] -> ([], prev_char) ];
               })
          (list, " ") len_list
      in
      ();
    end;
  end;
  Util.end_centered conf;
};

value print_wizards_by_date conf list = do {
  let sep_period_list =
    [(fun tm -> tm.Unix.tm_mon,
      fun tm ->
        let dmy =
          {year = tm.Unix.tm_year + 1900; month = tm.Unix.tm_mon + 1;
           day = 0; prec = Sure; delta = 0}
        in
        Wserver.wprint "%s"
          (capitale (Date.string_of_ondate conf (Dgreg dmy Dgregorian))));
     (fun tm -> tm.Unix.tm_year,
      fun tm -> Wserver.wprint "%d" (tm.Unix.tm_year + 1900))]
  in
  let list =
    List.sort (fun (_, _, _, mtm1) (_, _, _, mtm2) -> compare mtm2 mtm1)
      list
  in
  Wserver.wprint "<dl>\n<dt>";
  let _ =
    List.fold_left
      (fun (spl, prev) (wz, (wname, _), wfile, stm) -> do {
         let tm = Unix.localtime stm in
         let (new_item, spl) =
           match prev with
           [ Some prev_tm ->
               let (sep_period, _) =
                 match spl with
                 [ [sp :: _] -> sp
                 | [] -> assert False ]
               in
               if sep_period tm <> sep_period prev_tm then do {
                 Wserver.wprint "</dd>\n<dt>";
                 let spl =
                   match spl with
                   [ [_; (next_sp, _) :: _] ->
                       if next_sp tm <> next_sp prev_tm then List.tl spl
                       else spl
                   | _ -> spl ]
                 in
                 (True, spl)
               }
               else (False, spl)
           | None -> (True, spl) ]
         in
         if new_item then
           if stm = 0.0 then Wserver.wprint "....."
           else
             match spl with
             [ [(_, disp_sep_period) :: _] -> disp_sep_period tm
             | [] -> () ]
         else ();
         if new_item then Wserver.wprint "</dt>\n<dd>\n" else ();
         let wname = if wname = "" then wz else wname in
         Wserver.wprint "%s%t"
           (if prev = None || new_item then "" else ",\n")
           (fun _ ->
              if conf.wizard && conf.user = wz || wfile <> "" then
                Wserver.wprint "<a href=\"%sm=WIZNOTES;f=%s%t\">%s</a>"
                  (commd conf) (Util.code_varenv wz)
                  (fun _ ->
                     Wserver.wprint ";d=%d-%02d-%02d,%02d:%02d:%02d"
                       (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
                       tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
                       tm.Unix.tm_sec)
                  wname
              else Wserver.wprint "%s" wname);
         (spl, Some tm)
       })
      (sep_period_list, None) list
  in
  ();
  Wserver.wprint "</dd></dl>\n"
};

value print_old_wizards conf list =
  if list = [] then ()
  else do {
    tag "dl" begin
      tag "dd" "style=\"list-style-type:circle\"" begin
        Wserver.wprint "%s..." (transl_nth conf "and" 0);
        tag "dl" begin
          tag "dd" begin
            Mutil.list_iter_first
              (fun first wz -> do {
                 if not first then Wserver.wprint ",\n" else ();
                 stag "a" "href=\"%sm=WIZNOTES;f=%s\"" (commd conf)
                   (Util.code_varenv wz)
                 begin
                   for i = 0 to String.length wz - 1 do {
                     if wz.[i] = ' ' then Wserver.wprint "&nbsp;"
                     else Wserver.wprint "%c" wz.[i];
                   };
                 end
               })
              list;
            Wserver.wprint "\n";
          end;
        end;
      end;
    end;
  }
;

value wizard_list_from_dir conf base =
  match
    try Some (Sys.readdir (dir conf base)) with [ Sys_error _ -> None ]
  with
  [ Some arr ->
      List.fold_left
        (fun list fname ->
           if Filename.check_suffix fname ".txt" then
             let n = Filename.chop_extension fname in
             [n :: list]
           else list)
        [] (Array.to_list arr)
  | None -> [] ]
;

value print_search_form conf from_wiz =
  tag "table" begin
    tag "tr" begin
      tag "td" "align=\"%s\"" conf.right begin
        tag "form" "method=\"get\" action=\"%s\"" conf.command begin
          tag "p" begin
            hidden_env conf;
            xtag "input"
              "type=\"hidden\" name=\"m\" value=\"WIZNOTES_SEARCH\"";
            xtag "input"
              "name=\"s\" size=\"30\" maxlength=\"40\" value=\"%s\""
              (match p_getenv conf.env "s" with
               [ Some s -> quote_escaped s
               | None -> "" ]);
            if from_wiz <> "" then
              xtag "input" "type=\"hidden\" name=\"z\" value=\"%s\"" from_wiz
            else ();
            xtag "br";
            tag "label" begin
              xtag "input" "type=\"checkbox\" name=\"c\" value=\"on\"%s"
                (match p_getenv conf.env "c" with
                 [ Some "on" -> " checked=\"checked\""
                 | Some _ | None -> "" ]);
              Wserver.wprint "%s\n"
                (transl_nth conf "search/case sensitive" 1);
            end;
            xtag "input" "type=\"submit\" value=\"%s\""
              (capitale (transl_nth conf "search/case sensitive" 0));
          end;
        end;
      end;
    end;
  end
;

value print_main conf base auth_file =
  let wiztxt =
    Util.translate_eval (transl_nth conf "wizard/wizards/friend/friends" 1)
  in
  let title _ =
    Wserver.wprint "%s - %s" (capitale wiztxt)
      (Util.translate_eval (transl_nth conf "note/notes" 1))
  in
  let by_alphab_order = p_getenv conf.env "o" <> Some "H" in
  let wizdata =
    let list = read_auth_file auth_file in
    if by_alphab_order then
      List.sort
        (fun (_, (_, (o1, _))) (_, (_, (o2, _))) ->
           Gutil.alphabetic_order o1 o2)
        list
    else list
  in
  let wddir = dir conf base in
  do {
    Util.header_no_page_title conf title; (* mouais... *)
    print_link_to_welcome conf True;
    Wserver.wprint "<h1 style=\"text-align:center\" class=\"highlight\">";
    title False;
    Wserver.wprint "</h1>\n";
    let list =
      List.map
        (fun (wz, wname) ->
           let (wfile, wnote) = wiznote_date (wzfile wddir wz) in
           (wz, wname, wfile, wnote))
        wizdata
    in
    let old_list =
      let list = wizard_list_from_dir conf base in
      List.filter (fun n -> not (List.mem_assoc n wizdata)) list
    in
    if by_alphab_order then do {
      tag "p" begin
        Wserver.wprint "<em style=\"font-size:80%%\">\n";
        Wserver.wprint "%s " (capitale (transl conf "click"));
        Wserver.wprint "<a href=\"%sm=WIZNOTES;o=H\">%s</a>\n" (commd conf)
          (transl conf "here");
        Wserver.wprint "%s"
          (transl conf
             "for the list ordered by the date of the last modification");
        Wserver.wprint ".</em>\n";
      end;
      print_wizards_by_alphabetic_order conf list;
    }
    else print_wizards_by_date conf list;
    tag "p" begin
      Wserver.wprint "%d %s\n" (List.length wizdata) wiztxt;
    end;
    if by_alphab_order then do {
      print_old_wizards conf old_list;
      print_search_form conf "";
    }
    else ();
    trailer conf
  }
;

value wizard_page_title conf wz wizname h =
  Wserver.wprint "%s" wizname
;

value print_whole_wiznote conf base auth_file wz wfile (s, date) = do {
  let wizname =
    let wizdata = read_auth_file auth_file in
    try fst (List.assoc wz wizdata) with
    [ Not_found -> wz ]
  in
  let edit_opt =
    let can_edit = conf.wizard && conf.user = wz || conf.manitou in
    Some (can_edit, "WIZNOTES", code_varenv wz)
  in
  let title = wizard_page_title conf wz wizname in
  header_no_page_title conf title;
  print_link_to_welcome conf True;
  Wserver.wprint "<h1 style=\"text-align:center\" class=\"highlight\">";
  title False;
  Wserver.wprint "</h1>\n";
  match Util.open_etc_file "summary" with
  [ Some ic -> Templ.copy_from_templ conf [] ic
  | None -> () ];
  tag "table" "border=\"0\" width=\"100%%\"" begin
    tag "tr" begin
      tag "td" begin
        let s = string_with_macros conf [] s in
        let s =
          Wiki.html_with_summary_of_tlsw conf "NOTES"
            (Notes.file_path conf base) edit_opt s
        in
        Wserver.wprint "%s\n" s;
      end;
    end;
  end;
  if Sys.file_exists wfile then do {
    let tm = Unix.localtime date in
    let dmy =
      {day = tm.Unix.tm_mday; month = tm.Unix.tm_mon + 1;
       year = 1900 + tm.Unix.tm_year; prec = Sure; delta = 0}
    in
    tag "p" begin
      Wserver.wprint "<tt>(%s %02d:%02d)</tt>\n"
        (Date.string_of_ondate conf (Dgreg dmy Dgregorian))
        tm.Unix.tm_hour tm.Unix.tm_min;
    end
  }
  else ();
  match p_getenv conf.env "m" with
  [ Some "WIZNOTES_SEARCH" -> print_search_form conf wz
  | Some _ | None -> () ];
  trailer conf
};

value print_part_wiznote conf base wz s cnt0 =
  let title = wz in
  do {
    Util.header_no_page_title conf (fun _ -> Wserver.wprint "%s" title);
    let s = string_with_macros conf [] s in
    let lines = Wiki.extract_sub_part s cnt0 in
    let lines = if cnt0 = 0 then [title; "<br /><br />" :: lines] else lines in
    let file_path = Notes.file_path conf base in
    let can_edit = conf.wizard && conf.user = wz || conf.manitou in
    Wiki.print_sub_part conf can_edit file_path "NOTES" "WIZNOTES"
      (code_varenv wz) cnt0 lines;
    Util.trailer conf;
  }
;

value wizard_auth_file_name conf =
  match
    (p_getenv conf.base_env "wizard_descr_file",
     p_getenv conf.base_env "wizard_passwd_file")
  with
  [ (Some "" | None, Some "" | None) -> ""
  | (Some auth_file, _) -> auth_file
  | (_, Some auth_file) -> auth_file ]
;

value print conf base =
  let auth_file = wizard_auth_file_name conf in
  if auth_file = "" then incorrect_request conf
  else
    let f =
      (* backward compatibility *)
      match p_getenv conf.env "f" with
      [ None -> p_getenv conf.env "v"
      | x -> x ]
    in
    match f with
    [ Some wz ->
        let wz = Filename.basename wz in
        let wfile = wzfile (dir conf base) wz in
        let (s, date) = read_wizard_notes wfile in
        match p_getint conf.env "v" with
        [ Some cnt0 -> print_part_wiznote conf base wz s cnt0
        | None -> print_whole_wiznote conf base auth_file wz wfile (s, date) ]
    | None -> print_main conf base auth_file ]
;

value print_mod conf base =
  let auth_file =
    match
      (p_getenv conf.base_env "wizard_descr_file",
       p_getenv conf.base_env "wizard_passwd_file")
    with
    [ (Some "" | None, Some "" | None) -> ""
    | (Some auth_file, _) -> auth_file
    | (_, Some auth_file) -> auth_file ]
  in
  if auth_file = "" then incorrect_request conf
  else
    match p_getenv conf.env "f" with
    [ Some wz ->
        let wz = Filename.basename wz in
        let can_edit = conf.wizard && conf.user = wz || conf.manitou in
        if can_edit then
          let title = wizard_page_title conf wz wz in
          let wfile = wzfile (dir conf base) wz in
          let (s, _) = read_wizard_notes wfile in
          Wiki.print_mod_view_page conf True "WIZNOTES" wz title [] s
        else incorrect_request conf
    | None -> incorrect_request conf ]
;

value print_view conf base =
  let auth_file =
    match
      (p_getenv conf.base_env "wizard_descr_file",
       p_getenv conf.base_env "wizard_passwd_file")
    with
    [ (Some "" | None, Some "" | None) -> ""
    | (Some auth_file, _) -> auth_file
    | (_, Some auth_file) -> auth_file ]
  in
  if auth_file = "" then incorrect_request conf
  else
    match p_getenv conf.env "f" with
    [ Some wz ->
        let wz = Filename.basename wz in
        let title = wizard_page_title conf wz wz in
        let wfile = wzfile (dir conf base) wz in
        let (s, _) = read_wizard_notes wfile in
        Wiki.print_mod_view_page conf False "WIZNOTES" wz title [] s
    | None -> incorrect_request conf ]
;

value commit_wiznotes conf base wz s =
  let wddir = dir conf base in
  let fname = wzfile wddir wz in
  do {
    try Unix.mkdir wddir 0o755 with [ Unix.Unix_error _ _ _ -> () ];
    write_wizard_notes fname s;
    let pg = NotesLinks.PgWizard wz in
    Notes.update_notes_links_db conf pg s True;
  }
;

value print_mod_ok conf base =
  let auth_file =
    match
      (p_getenv conf.base_env "wizard_descr_file",
       p_getenv conf.base_env "wizard_passwd_file")
    with
    [ (Some "" | None, Some "" | None) -> ""
    | (Some auth_file, _) -> auth_file
    | (_, Some auth_file) -> auth_file ]
  in
  if auth_file = "" then incorrect_request conf
  else
    let fname =
      fun
      [ Some f -> f
      | None -> "nobody" ]
    in
    let edit_mode wz =
      if conf.wizard && conf.user = wz || conf.manitou then Some "WIZNOTES"
      else None
    in
    let mode = "NOTES" in
    let read_string wz =
      ([], fst (read_wizard_notes (wzfile (dir conf base) wz)))
    in
    let commit = commit_wiznotes conf base in
    let string_filter = string_with_macros conf [] in
    let file_path = Notes.file_path conf base in
    Wiki.print_mod_ok conf edit_mode mode fname read_string commit
      string_filter file_path False
;

value wizard_denying wddir =
  let fname = Filename.concat wddir "connected.deny" in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop [] where rec loop list =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some wname -> loop [wname :: list]
        | None -> do { close_in ic; List.rev list } ]
  | None -> [] ]
;

value do_connected_wizards conf base (_, _, _, wl) = do {
  let title _ =
    Wserver.wprint "%s"
      (capitale (transl_nth conf "wizard/wizards/friend/friends" 1))
  in
  header conf title;
  print_link_to_welcome conf True;
  let tm_now = Unix.time () in
  let wddir = dir conf base in
  let denying = wizard_denying wddir in
  let wl =
    if not (List.mem_assoc conf.user wl) then [(conf.user, tm_now) :: wl]
    else wl
  in
  let wl = List.sort (fun (_, tm1) (_, tm2) -> compare tm1 tm2) wl in
  let is_visible = not (List.mem conf.user denying) in
  tag "ul" begin
    let (not_everybody, _) =
      List.fold_left
        (fun (not_everybody, first) (wz, tm_user) ->
           if wz <> conf.user && List.mem wz denying && not conf.manitou
           then (True, first)
           else do {
             let (wfile, stm) = wiznote_date (wzfile wddir wz) in
             let tm = Unix.localtime stm in
             tag "li" "style=\"list-style-type:%s\""
               (if wz = conf.user && not is_visible ||
                   conf.manitou && List.mem wz denying
                then "circle"
                else "disc")
             begin
               let sec =
                 let d = tm_now -. tm_user in
                 if d = 0.0 then "" else Printf.sprintf " - %.0f s" d
               in
               if wfile <> "" then
                 Wserver.wprint
                   "<a href=\"%sm=WIZNOTES;f=%s%t\">%s</a>%s%s"
                   (commd conf) (Util.code_varenv wz)
                   (fun _ ->
                      Wserver.wprint ";d=%d-%02d-%02d,%02d:%02d:%02d"
                        (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
                        tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
                        tm.Unix.tm_sec)
                   wz sec
                   (if first then
                      " <span style=\"font-size:80%\">(" ^
                      transl conf "since the last click" ^ ")</span>"
                    else "")
               else Wserver.wprint "%s%s" wz sec;
               if wz = conf.user then do {
                 Wserver.wprint " :\n%s;"
                   (transl_nth conf "you are visible/you are not visible"
                      (if is_visible then 0 else 1));
                 Wserver.wprint
                   " %s %s%s%s %s" (transl conf "click")
                   (Printf.sprintf "<a href=\"%sm=CHANGE_WIZ_VIS;v=%d\">"
                      (commd conf) (if is_visible then 0 else 1))
                   (transl conf "here") "</a>" (transl conf "to change");
                 Wserver.wprint ".";
               }
               else ();
               Wserver.wprint "\n";
             end;
             (not_everybody, False)
           })
        (False, True) wl
    in
    if not_everybody then tag "li" begin Wserver.wprint "..."; end else ();
  end;
  trailer conf;
};

value connected_wizards conf base =
  match conf.n_connect with
  [ Some x -> do_connected_wizards conf base x
  | None -> incorrect_request conf ]
;

value do_change_wizard_visibility conf base x set_vis = do {
  let wddir = dir conf base in
  let denying = wizard_denying wddir in
  let is_visible = not (List.mem conf.user denying) in
  if not set_vis && not is_visible || set_vis && is_visible then ()
  else do {
    let tmp_file = Filename.concat wddir "1connected.deny" in
    let oc = Secure.open_out tmp_file in
    let found =
      List.fold_left
        (fun found wz ->
           if wz = conf.user && set_vis then True
           else do {
             Printf.fprintf oc "%s\n" wz;
             found
           })
        False denying
    in
    if not found && not set_vis then Printf.fprintf oc "%s\n" conf.user
    else ();
    close_out oc;
    let file = Filename.concat wddir "connected.deny" in
    Mutil.remove_file file;
    Sys.rename tmp_file file;
  };
  do_connected_wizards conf base x
};

value change_wizard_visibility conf base =
  match (conf.n_connect, p_getint conf.env "v") with
  [ (Some x, Some vis) -> do_change_wizard_visibility conf base x (vis <> 0)
  | _ -> incorrect_request conf ]
;

(* searching *)

value start_equiv_with case_sens s m i =
  let rec test i j =
    if j = String.length s then Some i
    else if i = String.length m then None
    else if case_sens then
      if m.[i] = s.[j] then test (i + 1) (j + 1) else None
    else
      match Name.next_chars_if_equiv m i s j with
      [ Some (i, j) -> test i j
      | None -> None ]
  in
  if case_sens then
    if m.[i] = s.[0] then test (i + 1) 1 else None
  else
    match Name.next_chars_if_equiv m i s 0 with
    [ Some (i, j) -> test i j
    | None -> None ]
;

value in_notes case_sens s m =
  loop False 0 where rec loop in_tag i =
    if i = String.length m then False
    else if in_tag then loop (m.[i] <> '>') (i + 1)
    else if m.[i] = '<' then loop True (i + 1)
    else
      match start_equiv_with case_sens s m i with
      [ Some _ -> True
      | None -> loop False (i + 1) ]
;

value html_highlight case_sens h s =
  let ht i j = "<span class=\"found\">" ^ String.sub s i (j - i) ^ "</span>" in
  loop False 0 0 where rec loop in_tag i len =
    if i = String.length s then Buff.get len
    else if in_tag then loop (s.[i] <> '>') (i + 1) (Buff.store len s.[i])
    else if s.[i] = '<' then loop True (i + 1) (Buff.store len s.[i])
    else
      match start_equiv_with case_sens h s i with
      [ Some j -> loop False j (Buff.mstore len (ht i j))
      | None -> loop False (i + 1) (Buff.store len s.[i]) ]
;

value print_found_wiznote conf base wz wfile (s, date) =
  let auth_file = wizard_auth_file_name conf in
  print_whole_wiznote conf base auth_file wz wfile (s, date)
;

value search_text conf base s =
  let s = if s = "" then " " else s in
  let list =
    let list = wizard_list_from_dir conf base in
    let list = List.sort compare list in
    match p_getenv conf.env "z" with
    [ Some "" | None -> list
    | Some wz ->
        loop list where rec loop =
          fun
          [ [wz1 :: list] -> if wz = wz1 then list else loop list
          | [] -> [] ] ]
  in
  let case_sens = p_getenv conf.env "c" = Some "on" in
  let wizo =
    loop list where rec loop =
      fun
      [ [] -> None
      | [wz :: list] ->
          let wz = Filename.basename wz in
          let wfile = wzfile (dir conf base) wz in
          let (nt, dt) = read_wizard_notes wfile in
          if in_notes case_sens s nt then Some (wz, wfile, nt, dt)
          else loop list ]
  in
  match wizo with
  [ Some (wz, wf, nt, dt) ->
      let nt = html_highlight case_sens s nt in
      print_found_wiznote conf base wz wf (nt, dt)
  | None -> print conf base ]
;

value print_search conf base =
  match try Some (List.assoc "s" conf.env) with [ Not_found -> None ] with
  [ Some s -> search_text conf base (Wserver.gen_decode False s)
  | None -> print conf base ]
;
