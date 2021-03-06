//  The code in this file was mechanically extracted from the TeX
//  source files of On Lisp.  Some operators are multiply defined,
//  as they were in the book.  Usually this means just that you get
//  an upwardly compatible version 2 of whatever it is.  Note, though,
//  that if you load this whole file you get:
//   1. the cltl1 versions of alrec and atrec.
//   2. varsym? defined as needed by the Prolog compiler.  So if you
//      want to use e.g. match with variables that begin with question
//      marks, comment out the final definition of varsym?
//  If you have questions or comments about this code, or you want
//  something I didn't include, send mail to onlisp@das.harvard.edu.
#f;

#f;

define method last1 (lst)
  head(copy-sequence(lst, start: size(lst) - 1));
end method last1;

define method single (lst)
  instance?(lst, <pair>) & ~ tail(lst);
end method single;

define method append1 (lst, obj)
  concatenate(lst, list(obj));
end method append1;

define method conc1 (lst, obj) concatenate!(lst, list(obj)); end method conc1;

define method mklist (obj)
  if (instance?(obj, <list>)) obj; else list(obj); end if;
end method mklist;

define method longer (x, y)
  local method compare (x, y)
          instance?(x, <pair>) & (empty?(y) | compare(tail(x), tail(y)));
        end method compare;
  if (instance?(x, <list>) & instance?(y, <list>))
    compare(x, y);
  else
    size(x) > size(y);
  end if;
end method longer;

define method filter (fn, lst)
  let acc = #f;
  for (x in lst) let val = fn(x); if (val) push!(val, acc); end if; end for;
  reverse!(acc);
end method filter;

define method group (source, n)
  if (zero?(n)) error("zero length"); end if;
  local method rec (source, acc)
          let rest = nth-tail(source, n);
          if (instance?(rest, <pair>))
            rec(rest, pair(copy-sequence(source, 0, n), acc));
          else
            reverse!(pair(source, acc));
          end if;
        end method rec;
  if (source) rec(source, #f); else #f; end if;
end method group;

define method flatten (x)
  local method rec (x, acc)
          if (empty?(x))
            acc;
          elseif (not(instance?(x, <list>)))
            pair(x, acc);
          else
            rec(head(x), rec(tail(x), acc));
          end if;
        end method rec;
  rec(x, #f);
end method flatten;

define method prune (test, tree)
  local method rec (tree, acc)
          if (empty?(tree))
            reverse!(acc);
          elseif (instance?(head(tree), <pair>))
            rec(tail(tree), pair(rec(head(tree), #f), acc));
          else
            rec(tail(tree),
                if (test(head(tree)))
                  acc;
                else
                  pair(head(tree), acc);
                end if);
          end if;
        end method rec;
  rec(tree, #f);
end method prune;

define method find2 (fn, lst)
  if (empty?(lst))
    #f;
  else
    let val = fn(head(lst));
    if (val) values(head(lst), val); else find2(fn, tail(lst)); end if;
  end if;
end method find2;

define method before (x, y, lst, #key test = \==)
  lst
   & begin
       let first = head(lst);
       if (test(y, first))
         #f;
       elseif (test(x, first))
         lst;
       else
         before(x, y, tail(lst), test: test);
       end if;
     end;
end method before;

define method after (x, y, lst, #key test = \==)
  let rest = before(y, x, lst, test: test);
  rest & member?(x, rest, test: test);
end method after;

define method duplicate (obj, lst, #key test = \==)
  member?(obj, tail(member?(obj, lst, test: test)), test: test);
end method duplicate;

define method split-if (fn, lst)
  let acc = #f;
  for (src = lst then cdr(src), until empty?(src) | fn(head(src)))
    push!(head(src), acc);
  finally
    values(reverse!(acc), src);
  end for;
end method split-if;

define method most (fn, lst)
  if (empty?(lst))
    values(#f, #f);
  else
    let wins = head(lst);
    let max = fn(wins);
    for (obj in tail(lst))
      let score = fn(obj);
      if (score > max) begin wins := obj; max := score; end; end if;
    end for;
    values(wins, max);
  end if;
end method most;

define method best (fn, lst)
  if (empty?(lst))
    #f;
  else
    let wins = head(lst);
    for (obj in tail(lst)) if (fn(obj, wins)) wins := obj; end if; end for;
    wins;
  end if;
end method best;

define method mostn (fn, lst)
  if (empty?(lst))
    values(#f, #f);
  else
    let result = list(head(lst));
    let max = fn(head(lst));
    for (obj in tail(lst))
      let score = fn(obj);
      if (score > max)
        begin max := score; result := list(obj); end;
      elseif (score = max)
        push!(obj, result);
      end if;
    end for;
    values(reverse!(result), max);
  end if;
end method mostn;

define method map0-n (fn, n) mapa-b(fn, 0, n); end method map0-n;

define method map1-n (fn, n) mapa-b(fn, 1, n); end method map1-n;

define method mapa-b (fn, a, b, #key step = 1)
  for (i = a then i + step, result = nil then nil, until i > b)
    push!(fn(i), result);
  finally
    reverse!(result);
  end for;
end method mapa-b;

define method map-> (fn, start, test-fn, succ-fn)
  for (i = start then funcall(succ-fn, i), result = nil then nil,
       until test-fn(i))
    push!(fn(i), result);
  finally
    reverse!(result);
  end for;
end method map->;

define method mappend (fn, #rest lsts)
  apply(concatenate,
        apply(// LTD: Can't convert complex function MAPCAR.
              mapcar, fn, lsts));
end method mappend;

define method mapcars (fn, #rest lsts)
  let result = #f;
  for (lst in lsts) for (obj in lst) push!(fn(obj), result); end for; end for;
  reverse!(result);
end method mapcars;

define method rmapcar (fn, #rest args)
  if (any?(method (x) not(instance?(x, <list>)); end method, args))
    apply(fn, args);
  else
    apply(// LTD: Can't convert complex function MAPCAR.
          mapcar, method (#rest args) apply(rmapcar, fn, args); end method,
          args);
  end if;
end method rmapcar;

define method readlist (#rest args)
  values(// LTD: Function READ-FROM-STRING not yet implemented.
         read-from-string(concatenate-as(<string>,
                                         "(",
                                         apply(// LTD: Can't convert complex function READ-LINE.
                                               read-line,
                                               args),
                                         ")")));
end method readlist;

define method prompt (#rest args)
  apply(format, *query-io*, args);
  // LTD: Function READ not yet implemented.
  read(*query-io*);
end method prompt;

define method break-loop (fn, quit, #rest args)
  format(*query-io*, "Entering break-loop.\n");
  block (return)
    while (#t)
      let in = apply(prompt, args);
      if (quit(in))
        return(#f);
      else
        format(*query-io*, "%S\n", fn(in));
      end if;
    end while;
  end block;
end method break-loop;

define method mkstr (#rest args)
  let s
      = // LTD: Function MAKE-STRING-OUTPUT-STREAM not yet implemented.
        make-string-output-stream(element-type: #f);
  block (nil)
    begin for (a in args) print(a, s); end for; end;
  cleanup
    close(s);
  end block;
  // LTD: Function GET-OUTPUT-STREAM-STRING not yet implemented.
  get-output-stream-string(s);
end method mkstr;

define method symb (#rest args)
  values(as(<symbol>, apply(mkstr, args)));
end method symb;

define method reread (#rest args)
  values(// LTD: Function READ-FROM-STRING not yet implemented.
         read-from-string(apply(mkstr, args)));
end method reread;

define method explode (sym)
  map-as(<list>,
         method (c)
           as(<symbol>, make(<string>, size: 1, fill: c));
         end method,
         as(<string>, sym));
end method explode;

define variable *!equivs* = make(<table>);

define method ! (fn) *!equivs*[fn] | fn; end method !;

define method def! (fn, fn!) *!equivs*[fn] := fn!; end method def!;

define method memoize (fn)
  let cache = make(<table>, test: \=);
  method (#rest args)
    let (val, win) = cache[args];
    if (win) val; else cache[args] := apply(fn, args); end if;
  end method;
end method memoize;

define method compose (#rest fns)
  if (fns)
    let fn1 = head(copy-sequence(fns, start: size(fns) - 1));
    let fns = copy-sequence(fns, size(fns) - 1);
    method (#rest args)
      reduce(// LTD: Can't convert complex function FUNCALL.
             funcall, apply(fn1, args), fns);
    end method;
  else
    identity;
  end if;
end method compose;

define method fif (if, then, #key else)
  method (x)
    if (if (x); end if) then(x); elseif (else) else(x); end if;
  end method;
end method fif;

define method fint (fn, #rest fns)
  if (empty?(fns))
    fn;
  else
    let chain = apply(fint, fns);
    method (x) fn(x) & chain(x); end method;
  end if;
end method fint;

define method fun (fn, #rest fns)
  if (empty?(fns))
    fn;
  else
    let chain = apply(fun, fns);
    method (x) fn(x) | chain(x); end method;
  end if;
end method fun;

define method lrec (rec, #key base)
  local method self (lst)
          if (empty?(lst))
            if (instance?(base, <function>)) base(); else base; end if;
          else
            rec(head(lst), method () self(tail(lst)); end method);
          end if;
        end method self;
  self;
end method lrec;

define method rfind-if (fn, tree)
  if (not(instance?(tree, <list>)))
    fn(tree) & tree;
  else
    rfind-if(fn, head(tree))
     | if (tail(tree)) rfind-if(fn, tail(tree)); end if;
  end if;
end method rfind-if;

define method ttrav (rec, #key base = identity)
  local method self (tree)
          if (not(instance?(tree, <list>)))
            if (instance?(base, <function>)) base(tree); else base; end if;
          else
            rec(self(head(tree)), if (tail(tree)) self(tail(tree)); end if);
          end if;
        end method self;
  self;
end method ttrav;

define method trec (rec, #key base = identity)
  local method self (tree)
          if (not(instance?(tree, <list>)))
            if (instance?(base, <function>)) base(tree); else base; end if;
          else
            rec(tree, method () self(head(tree)); end method,
                method ()
                  if (tail(tree)) self(tail(tree)); end if;
                end method);
          end if;
        end method self;
  self;
end method trec;

// LTD: No macros.
#"mac";

// LTD: No macros.
#"when-bind";

// LTD: No macros.
#"when-bind*";

// LTD: No macros.
#"with-gensyms";

// LTD: No macros.
#"condlet";

define method condlet-clause (vars, cl, bodfn)
  list(head(cl),
       list(#"let", map(tail, vars),
            list(#"let", condlet-binds(vars, cl),
                 pair(bodfn, map(tail, vars)))));
end method condlet-clause;

define method condlet-binds (vars, cl)
  map(method (bindform)
        if (instance?(bindform, <pair>))
          pair(tail(cl-assoc(head(bindform), vars)), tail(bindform));
        end if;
      end method,
      tail(cl));
end method condlet-binds;

// LTD: No macros.
#"if3";

// LTD: No macros.
#"nif";

// LTD: No macros.
#"in";

// LTD: No macros.
#"inq";

// LTD: No macros.
#"in-if";

// LTD: No macros.
#">case";

define method >casex (g, cl)
  let key = head(cl);
  let rest = tail(cl);
  if (instance?(key, <pair>))
    pair(apply(list, #"in", g, key), rest);
  elseif (inq(key, #t, otherwise))
    pair(#t, rest);
  else
    error("bad >case clause");
  end if;
end method >casex;

// LTD: No macros.
#"while";

// LTD: No macros.
#"till";

// LTD: No macros.
#"for";

// LTD: No macros.
#"do-tuples/o";

// LTD: No macros.
#"do-tuples/c";

define method dt-args (len, rest, src)
  map0-n(method (m)
           map1-n(method (n)
                    let x = m + n;
                    if (x >= len)
                      list(#"nth", x - len, src);
                    else
                      list(#"nth", x - 1, rest);
                    end if;
                  end method,
                  len);
         end method,
         len - 2);
end method dt-args;

// LTD: No macros.
#"mvdo*";

define method mvdo-gen (binds, rebinds, test, body)
  if (empty?(binds))
    let label = generate-symbol();
    apply(list, #"prog", #f, label,
          list(#"if", head(test),
               list(#"return", pair(#"progn", tail(test)))),
          concatenate(body, mvdo-rebind-gen(rebinds),
                      list(list(#"go", label))));
  else
    let rec = mvdo-gen(tail(binds), rebinds, test, body);
    let var/s = head(head(binds));
    let expr = cadar(binds);
    if (not(instance?(var/s, <list>)))
      list(#"let", list(list(var/s, expr)), rec);
    else
      list(#"multiple-value-bind", var/s, expr, rec);
    end if;
  end if;
end method mvdo-gen;

define method mvdo-rebind-gen (rebinds)
  if (empty?(rebinds))
    #f;
  elseif (size(head(rebinds)) < 3)
    mvdo-rebind-gen(tail(rebinds));
  else
    pair(list(if (not(instance?(head(head(rebinds)), <list>)))
                #"setq";
              else
                #"multiple-value-setq";
              end if,
              head(head(rebinds)), third(head(rebinds))),
         mvdo-rebind-gen(tail(rebinds)));
  end if;
end method mvdo-rebind-gen;

// LTD: No macros.
#"mvpsetq";

define method shuffle (x, y)
  if (empty?(x))
    y;
  elseif (empty?(y))
    x;
  else
    apply(list, head(x), head(y), shuffle(tail(x), tail(y)));
  end if;
end method shuffle;

// LTD: No macros.
#"mvdo";

// LTD: No macros.
#"allf";

// LTD: No macros.
#"nilf";

// LTD: No macros.
#"tf";

// LTD: No macros.
#"toggle";

begin
  fluid-bind (*function-name*
               = generate-subform-name(#"toggle2", *function-name*))
    fluid-bind (*function-parent*
                 = tlf-function-parent(#(#"quote", #"toggle2")))
      record-sf-eval(compiler-eval(*function-name*),
                     compiler-eval(*function-parent*));
      record-sf-compile(*function-name*, *function-parent*);
      set-macro-function(#"toggle2",
                         method (%%macroarg%%, environment)
                           let &whole138274 = %%macroarg%%;
                           let (%reference)138275 = tail(&whole138274);
                           let check-lambda-list-top-level138276
                               = check-lambda-list-top-level(#(#"%reference"),
                                                             &whole138274,
                                                             (%reference)138275,
                                                             1,
                                                             1,
                                                             #(),
                                                             #"macro");
                           let %reference = head((%reference)138275);
                           begin
                             #f;
                             let (dummies, vals, newval, setter, getter)
                                 = // LTD: Function GET-SETF-METHOD not yet implemented.
                                   get-setf-method(%reference, environment);
                             for (d = dummies then cdr(d),
                                  v = vals then cdr(v),
                                  let-list = nil then cons(list(car(d),
                                                                car(v)),
                                                           let-list),
                                  until empty?(d))
                               #f;
                             finally
                               values(push!(list(head(newval),
                                                 list(#"not", getter)),
                                            let-list),
                                      list(#"let*",
                                           reverse!(let-list),
                                           setter));
                             end for;
                           end;
                         end method);
      broadcast-redefined(#"toggle2",
                          macro: #(#(#"let*",
                                     #(#(#"&whole138274", #"%%macroarg%%"),
                                       #(#"(%reference)138275",
                                         #(#"cdr", #"&whole138274")),
                                       #(#"check-lambda-list-top-level138276",
                                         #(#"check-lambda-list-top-level",
                                           #(#"quote", #(#"%reference")),
                                           #"&whole138274",
                                           #"(%reference)138275",
                                           1,
                                           1,
                                           #(#"quote", #()),
                                           #"macro")),
                                       #(#"%reference",
                                         #(#"car",
                                           #(#"the-cons",
                                             #"(%reference)138275")))),
                                     #(#"block",
                                       #"toggle2",
                                       #(),
                                       #(#"multiple-value-bind",
                                         #(#"dummies",
                                           #"vals",
                                           #"newval",
                                           #"setter",
                                           #"getter"),
                                         #(#"get-setf-method",
                                           #"%reference",
                                           #"environment"),
                                         #(#"do",
                                           #(#(#"d",
                                               #"dummies",
                                               #(#"cdr", #"d")),
                                             #(#"v",
                                               #"vals",
                                               #(#"cdr", #"v")),
                                             #(#"let-list",
                                               #(),
                                               #(#"cons",
                                                 #(#"list",
                                                   #(#"car", #"d"),
                                                   #(#"car", #"v")),
                                                 #"let-list"))),
                                           #(#(#"null", #"d"),
                                             #(#"push",
                                               #(#"list",
                                                 #(#"car", #"newval"),
                                                 #(#"list",
                                                   #(#"quote", #"not"),
                                                   #"getter")),
                                               #"let-list"),
                                             #(#"list",
                                               #(#"quote", #"let*"),
                                               #(#"nreverse", #"let-list"),
                                               #"setter"))))))));
      symbol-remove-property(#"toggle2", #"%fun-documentation");
      flag-symbol-macro$symbol(#"toggle2");
    end fluid-bind;
  end fluid-bind;
  #"toggle2";
end;

begin
  fluid-bind (*function-name*
               = generate-subform-name(#"concf", *function-name*))
    fluid-bind (*function-parent*
                 = tlf-function-parent(#(#"quote", #"concf")))
      record-sf-eval(compiler-eval(*function-name*),
                     compiler-eval(*function-parent*));
      record-sf-compile(*function-name*, *function-parent*);
      set-macro-function(#"concf",
                         method (%%macroarg%%, environment)
                           let &whole138365 = %%macroarg%%;
                           let (%reference ...)138366 = tail(&whole138365);
                           let check-lambda-list-top-level138368
                               = check-lambda-list-top-level(#(#"%reference",
                                                               #"obj"),
                                                             &whole138365,
                                                             (%reference ...)138366,
                                                             2,
                                                             2,
                                                             #(),
                                                             #"macro");
                           let %reference = head((%reference ...)138366);
                           let (obj)138367 = tail((%reference ...)138366);
                           let obj = head((obj)138367);
                           begin
                             #f;
                             let (dummies, vals, newval, setter, getter)
                                 = // LTD: Function GET-SETF-METHOD not yet implemented.
                                   get-setf-method(%reference, environment);
                             for (d = dummies then cdr(d),
                                  v = vals then cdr(v),
                                  let-list = nil then cons(list(car(d),
                                                                car(v)),
                                                           let-list),
                                  until empty?(d))
                               #f;
                             finally
                               values(push!(list(head(newval),
                                                 list(#"nconc", getter, obj)),
                                            let-list),
                                      list(#"let*",
                                           reverse!(let-list),
                                           setter));
                             end for;
                           end;
                         end method);
      broadcast-redefined(#"concf",
                          macro: #(#(#"let*",
                                     #(#(#"&whole138365", #"%%macroarg%%"),
                                       #(#"(%reference ...)138366",
                                         #(#"cdr", #"&whole138365")),
                                       #(#"check-lambda-list-top-level138368",
                                         #(#"check-lambda-list-top-level",
                                           #(#"quote",
                                             #(#"%reference", #"obj")),
                                           #"&whole138365",
                                           #"(%reference ...)138366",
                                           2,
                                           2,
                                           #(#"quote", #()),
                                           #"macro")),
                                       #(#"%reference",
                                         #(#"car",
                                           #(#"the-cons",
                                             #"(%reference ...)138366"))),
                                       #(#"(obj)138367",
                                         #(#"cdr",
                                           #(#"the-cons",
                                             #"(%reference ...)138366"))),
                                       #(#"obj",
                                         #(#"car",
                                           #(#"the-cons", #"(obj)138367")))),
                                     #(#"block",
                                       #"concf",
                                       #(),
                                       #(#"multiple-value-bind",
                                         #(#"dummies",
                                           #"vals",
                                           #"newval",
                                           #"setter",
                                           #"getter"),
                                         #(#"get-setf-method",
                                           #"%reference",
                                           #"environment"),
                                         #(#"do",
                                           #(#(#"d",
                                               #"dummies",
                                               #(#"cdr", #"d")),
                                             #(#"v",
                                               #"vals",
                                               #(#"cdr", #"v")),
                                             #(#"let-list",
                                               #(),
                                               #(#"cons",
                                                 #(#"list",
                                                   #(#"car", #"d"),
                                                   #(#"car", #"v")),
                                                 #"let-list"))),
                                           #(#(#"null", #"d"),
                                             #(#"push",
                                               #(#"list",
                                                 #(#"car", #"newval"),
                                                 #(#"list",
                                                   #(#"quote", #"nconc"),
                                                   #"getter",
                                                   #"obj")),
                                               #"let-list"),
                                             #(#"list",
                                               #(#"quote", #"let*"),
                                               #(#"nreverse", #"let-list"),
                                               #"setter"))))))));
      symbol-remove-property(#"concf", #"%fun-documentation");
      flag-symbol-macro$symbol(#"concf");
    end fluid-bind;
  end fluid-bind;
  #"concf";
end;

begin
  fluid-bind (*function-name*
               = generate-subform-name(#"conc1f", *function-name*))
    fluid-bind (*function-parent*
                 = tlf-function-parent(#(#"quote", #"conc1f")))
      record-sf-eval(compiler-eval(*function-name*),
                     compiler-eval(*function-parent*));
      record-sf-compile(*function-name*, *function-parent*);
      set-macro-function(#"conc1f",
                         method (%%macroarg%%, environment)
                           let &whole138473 = %%macroarg%%;
                           let (%reference ...)138474 = tail(&whole138473);
                           let check-lambda-list-top-level138476
                               = check-lambda-list-top-level(#(#"%reference",
                                                               #"obj"),
                                                             &whole138473,
                                                             (%reference ...)138474,
                                                             2,
                                                             2,
                                                             #(),
                                                             #"macro");
                           let %reference = head((%reference ...)138474);
                           let (obj)138475 = tail((%reference ...)138474);
                           let obj = head((obj)138475);
                           begin
                             #f;
                             let (dummies, vals, newval, setter, getter)
                                 = // LTD: Function GET-SETF-METHOD not yet implemented.
                                   get-setf-method(%reference, environment);
                             for (d = dummies then cdr(d),
                                  v = vals then cdr(v),
                                  let-list = nil then cons(list(car(d),
                                                                car(v)),
                                                           let-list),
                                  until empty?(d))
                               #f;
                             finally
                               values(push!(list(head(newval),
                                                 list(#(#"lambda",
                                                        #(#"place", #"obj"),
                                                        #(#"nconc",
                                                          #"place",
                                                          #(#"list",
                                                            #"obj"))),
                                                      getter,
                                                      obj)),
                                            let-list),
                                      list(#"let*",
                                           reverse!(let-list),
                                           setter));
                             end for;
                           end;
                         end method);
      broadcast-redefined(#"conc1f",
                          macro: #(#(#"let*",
                                     #(#(#"&whole138473", #"%%macroarg%%"),
                                       #(#"(%reference ...)138474",
                                         #(#"cdr", #"&whole138473")),
                                       #(#"check-lambda-list-top-level138476",
                                         #(#"check-lambda-list-top-level",
                                           #(#"quote",
                                             #(#"%reference", #"obj")),
                                           #"&whole138473",
                                           #"(%reference ...)138474",
                                           2,
                                           2,
                                           #(#"quote", #()),
                                           #"macro")),
                                       #(#"%reference",
                                         #(#"car",
                                           #(#"the-cons",
                                             #"(%reference ...)138474"))),
                                       #(#"(obj)138475",
                                         #(#"cdr",
                                           #(#"the-cons",
                                             #"(%reference ...)138474"))),
                                       #(#"obj",
                                         #(#"car",
                                           #(#"the-cons", #"(obj)138475")))),
                                     #(#"block",
                                       #"conc1f",
                                       #(),
                                       #(#"multiple-value-bind",
                                         #(#"dummies",
                                           #"vals",
                                           #"newval",
                                           #"setter",
                                           #"getter"),
                                         #(#"get-setf-method",
                                           #"%reference",
                                           #"environment"),
                                         #(#"do",
                                           #(#(#"d",
                                               #"dummies",
                                               #(#"cdr", #"d")),
                                             #(#"v",
                                               #"vals",
                                               #(#"cdr", #"v")),
                                             #(#"let-list",
                                               #(),
                                               #(#"cons",
                                                 #(#"list",
                                                   #(#"car", #"d"),
                                                   #(#"car", #"v")),
                                                 #"let-list"))),
                                           #(#(#"null", #"d"),
                                             #(#"push",
                                               #(#"list",
                                                 #(#"car", #"newval"),
                                                 #(#"list",
                                                   #(#"quote",
                                                     #(#"lambda",
                                                       #(#"place", #"obj"),
                                                       #(#"nconc",
                                                         #"place",
                                                         #(#"list",
                                                           #"obj")))),
                                                   #"getter",
                                                   #"obj")),
                                               #"let-list"),
                                             #(#"list",
                                               #(#"quote", #"let*"),
                                               #(#"nreverse", #"let-list"),
                                               #"setter"))))))));
      symbol-remove-property(#"conc1f", #"%fun-documentation");
      flag-symbol-macro$symbol(#"conc1f");
    end fluid-bind;
  end fluid-bind;
  #"conc1f";
end;

begin
  fluid-bind (*function-name*
               = generate-subform-name(#"concnew", *function-name*))
    fluid-bind (*function-parent*
                 = tlf-function-parent(#(#"quote", #"concnew")))
      record-sf-eval(compiler-eval(*function-name*),
                     compiler-eval(*function-parent*));
      record-sf-compile(*function-name*, *function-parent*);
      set-macro-function(#"concnew",
                         method (%%macroarg%%, environment)
                           let &whole138581 = %%macroarg%%;
                           let (%reference ...)138582 = tail(&whole138581);
                           let check-lambda-list-top-level138585
                               = check-lambda-list-top-level(#(#"%reference",
                                                               #"obj",
                                                               #"&rest",
                                                               #"args"),
                                                             &whole138581,
                                                             (%reference ...)138582,
                                                             2,
                                                             2,
                                                             #"t",
                                                             #"macro");
                           let %reference = head((%reference ...)138582);
                           let (obj ...)138583 = tail((%reference ...)138582);
                           let obj = head((obj ...)138583);
                           let args138584 = tail((obj ...)138583);
                           let args = args138584;
                           begin
                             #f;
                             let (dummies, vals, newval, setter, getter)
                                 = // LTD: Function GET-SETF-METHOD not yet implemented.
                                   get-setf-method(%reference, environment);
                             for (d = dummies then cdr(d),
                                  v = vals then cdr(v),
                                  let-list = nil then cons(list(car(d),
                                                                car(v)),
                                                           let-list),
                                  until empty?(d))
                               #f;
                             finally
                               values(push!(list(head(newval),
                                                 apply(list,
                                                       #(#"lambda",
                                                         #(#"place",
                                                           #"obj",
                                                           #"&rest",
                                                           #"args"),
                                                         #(#"unless",
                                                           #(#"apply",
                                                             #(#"function",
                                                               #"member"),
                                                             #"obj",
                                                             #"place",
                                                             #"args"),
                                                           #(#"nconc",
                                                             #"place",
                                                             #(#"list",
                                                               #"obj")))),
                                                       getter,
                                                       obj,
                                                       args)),
                                            let-list),
                                      list(#"let*",
                                           reverse!(let-list),
                                           setter));
                             end for;
                           end;
                         end method);
      broadcast-redefined(#"concnew",
                          macro: #(#(#"let*",
                                     #(#(#"&whole138581", #"%%macroarg%%"),
                                       #(#"(%reference ...)138582",
                                         #(#"cdr", #"&whole138581")),
                                       #(#"check-lambda-list-top-level138585",
                                         #(#"check-lambda-list-top-level",
                                           #(#"quote",
                                             #(#"%reference",
                                               #"obj",
                                               #"&rest",
                                               #"args")),
                                           #"&whole138581",
                                           #"(%reference ...)138582",
                                           2,
                                           2,
                                           #(#"quote", #"t"),
                                           #"macro")),
                                       #(#"%reference",
                                         #(#"car",
                                           #(#"the-cons",
                                             #"(%reference ...)138582"))),
                                       #(#"(obj ...)138583",
                                         #(#"cdr",
                                           #(#"the-cons",
                                             #"(%reference ...)138582"))),
                                       #(#"obj",
                                         #(#"car",
                                           #(#"the-cons",
                                             #"(obj ...)138583"))),
                                       #(#"args138584",
                                         #(#"cdr",
                                           #(#"the-cons",
                                             #"(obj ...)138583"))),
                                       #(#"args", #"args138584")),
                                     #(#"block",
                                       #"concnew",
                                       #(),
                                       #(#"multiple-value-bind",
                                         #(#"dummies",
                                           #"vals",
                                           #"newval",
                                           #"setter",
                                           #"getter"),
                                         #(#"get-setf-method",
                                           #"%reference",
                                           #"environment"),
                                         #(#"do",
                                           #(#(#"d",
                                               #"dummies",
                                               #(#"cdr", #"d")),
                                             #(#"v",
                                               #"vals",
                                               #(#"cdr", #"v")),
                                             #(#"let-list",
                                               #(),
                                               #(#"cons",
                                                 #(#"list",
                                                   #(#"car", #"d"),
                                                   #(#"car", #"v")),
                                                 #"let-list"))),
                                           #(#(#"null", #"d"),
                                             #(#"push",
                                               #(#"list",
                                                 #(#"car", #"newval"),
                                                 #(#"list*",
                                                   #(#"quote",
                                                     #(#"lambda",
                                                       #(#"place",
                                                         #"obj",
                                                         #"&rest",
                                                         #"args"),
                                                       #(#"unless",
                                                         #(#"apply",
                                                           #(#"function",
                                                             #"member"),
                                                           #"obj",
                                                           #"place",
                                                           #"args"),
                                                         #(#"nconc",
                                                           #"place",
                                                           #(#"list",
                                                             #"obj"))))),
                                                   #"getter",
                                                   #"obj",
                                                   #"args")),
                                               #"let-list"),
                                             #(#"list",
                                               #(#"quote", #"let*"),
                                               #(#"nreverse", #"let-list"),
                                               #"setter"))))))));
      symbol-remove-property(#"concnew", #"%fun-documentation");
      flag-symbol-macro$symbol(#"concnew");
    end fluid-bind;
  end fluid-bind;
  #"concnew";
end;

// LTD: No macros.
#"_f";

// LTD: No macros.
#"pull";

// LTD: No macros.
#"pull-if";

// LTD: No macros.
#"popn";

// LTD: No macros.
#"sortf";

// LTD: No macros.
#"aif";

// LTD: No macros.
#"awhen";

// LTD: No macros.
#"awhile";

// LTD: No macros.
#"aand";

// LTD: No macros.
#"acond";

// LTD: No macros.
#"alambda";

// LTD: No macros.
#"ablock";

// LTD: No macros.
#"aif2";

// LTD: No macros.
#"awhen2";

// LTD: No macros.
#"awhile2";

// LTD: No macros.
#"acond2";

begin
  let g = generate-symbol();
  define method read2 (#key str = *standard-input*)
    let val
        = // LTD: Function READ not yet implemented.
          read(str, #f, g);
    if (~ (val = g)) values(val, #t); end if;
  end method read2;
end;

// LTD: No macros.
#"do-file";

// LTD: No macros.
#"fn";

define method rbuild (expr)
  if (not(instance?(expr, <list>)) | head(expr) == #"lambda")
    expr;
  elseif (head(expr) == #"compose")
    build-compose(tail(expr));
  else
    build-call(head(expr), tail(expr));
  end if;
end method rbuild;

define method build-call (op, fns)
  let g = generate-symbol();
  list(#"lambda", list(g),
       pair(op, map(method (f) list(rbuild(f), g); end method, fns)));
end method build-call;

define method build-compose (fns)
  let g = generate-symbol();
  list(#"lambda", list(g),
       local method rec (fns)
               if (fns)
                 list(rbuild(head(fns)), rec(tail(fns)));
               else
                 g;
               end if;
             end method rec;
         rec(fns));
end method build-compose;

// LTD: No macros.
#"alrec";

// LTD: No macros.
#"alrec";

// LTD: No macros.
#"on-cdrs";

define method unions (#rest sets)
  on-cdrs(union(it, rec), head(sets), tail(sets));
end method unions;

define method intersections (#rest sets)
  if (~ any?(empty?, sets))
    on-cdrs(intersection(it, rec), head(sets), tail(sets));
  end if;
end method intersections;

define method differences (set, #rest outs)
  on-cdrs(set-difference(rec, it), set, outs);
end method differences;

define method maxmin (args)
  if (args)
    on-cdrs(let (mx, mn) = rec;
              values(max(mx, it), min(mn, it)),
              values(head(args), head(args)), tail(args));
  end if;
end method maxmin;

// LTD: No macros.
#"atrec";

// LTD: No macros.
#"atrec";

// LTD: No macros.
#"on-trees";

define constant unforced = generate-symbol();

define class <delay> (<object>)
  slot delay-forced, init-keyword: #"delay-forced";
  slot delay-closure, init-keyword: #"delay-closure";
end class <delay>;

// LTD: No macros.
#"delay";

define method force (x)
  if (delay-p(x))
    if (x.delay-forced == unforced)
      (x.delay-closure)();
    else
      x.delay-forced;
    end if;
  else
    x;
  end if;
end method force;

// LTD: No macros.
#"abbrev";

// LTD: No macros.
#"abbrevs";

// LTD: No macros.
#"propmacro";

// LTD: No macros.
#"propmacros";

// LTD: No macros.
#"defanaph";

define method anaphex (args, expr)
  if (args)
    let sym = generate-symbol();
    list(#"let*", list(list(sym, head(args)), list(#"it", sym)),
         anaphex(tail(args), concatenate(expr, list(sym))));
  else
    expr;
  end if;
end method anaphex;

define method pop-symbol (sym)
  as(<symbol>, copy-sequence(as(<string>, sym), 1));
end method pop-symbol;

// LTD: No macros.
#"defanaph";

define method anaphex1 (args, call)
  if (args)
    let sym = generate-symbol();
    list(#"let*", list(list(sym, head(args)), list(#"it", sym)),
         anaphex1(tail(args), concatenate(call, list(sym))));
  else
    call;
  end if;
end method anaphex1;

define method anaphex2 (op, args)
  list(#"let", list(list(#"it", head(args))),
       apply(list, op, #"it", tail(args)));
end method anaphex2;

define method anaphex3 (op, args)
  list(#"_f", list(#"lambda", #(#"it"), apply(list, op, #"it", tail(args))),
       head(args));
end method anaphex3;

// LTD: No macros.
#"defdelim";

begin
  let rpar
      = // LTD: Function GET-MACRO-CHARACTER not yet implemented.
        get-macro-character(')');
  define method ddfn (left, right, fn)
    // LTD: Function SET-MACRO-CHARACTER not yet implemented.
    set-macro-character(right, rpar);
    set-dispatch-macro-character('#', left,
                                 method (stream, char1, char2)
                                   apply(fn,
                                         // LTD: Function READ-DELIMITED-LIST not yet implemented.
                                         read-delimited-list(right,
                                                             stream,
                                                             #t));
                                 end method);
  end method ddfn;
end;

// LTD: No macros.
#"dbind";

define method destruc (pat, seq,
                       #key atom?
                             = method (x)
                                 not(instance?(x, <list>));
                               end method,
                       n = 0)
  if (empty?(pat))
    #f;
  else
    let rest
        = if (atom?(pat))
            pat;
          elseif (head(pat) == #"&rest")
            second(pat);
          elseif (head(pat) == #"&body")
            second(pat);
          else
            #f;
          end if;
    if (rest)
      list(list(rest, list(#"subseq", seq, n)));
    else
      let p = head(pat);
      let rec = destruc(tail(pat), seq, atom?, n + 1);
      if (atom?(p))
        pair(list(p, list(#"elt", seq, n)), rec);
      else
        let var = generate-symbol();
        pair(pair(list(var, list(#"elt", seq, n)), destruc(p, var, atom?)),
             rec);
      end if;
    end if;
  end if;
end method destruc;

define method dbind-ex (binds, body)
  if (empty?(binds))
    pair(#"progn", body);
  else
    list(#"let",
         map(method (b)
               if (instance?(head(b), <pair>)) head(b); else b; end if;
             end method,
             binds),
         dbind-ex(apply(concatenate!,
                        map(method (b)
                              if (instance?(head(b), <pair>)) tail(b); end if;
                            end method,
                            binds)),
                  body));
  end if;
end method dbind-ex;

// LTD: No macros.
#"with-matrix";

// LTD: No macros.
#"with-array";

// LTD: No macros.
#"with-struct";

// LTD: No macros.
#"with-places";

define method wplac-ex (binds, body)
  if (empty?(binds))
    pair(#"progn", body);
  else
    list(#"symbol-macrolet",
         map(method (b)
               if (instance?(head(b), <pair>)) head(b); else b; end if;
             end method,
             binds),
         wplac-ex(apply(concatenate!,
                        map(method (b)
                              if (instance?(head(b), <pair>)) tail(b); end if;
                            end method,
                            binds)),
                  body));
  end if;
end method wplac-ex;

define method match (x, y, #key binds)
  acond2((x == y | x == #"_" | y == #"_")(values(binds, #t)),
         (binding(x, binds))(match(it, y, binds)),
         (binding(y, binds))(match(x, it, binds)),
         (varsym?(x))(values(pair(pair(x, y), binds), #t)),
         (varsym?(y))(values(pair(pair(y, x), binds), #t)),
         (instance?(x, <pair>) & instance?(y, <pair>)
           & match(head(x), head(y), binds))(match(tail(x), tail(y), it)),
         t(values(#f, #f)));
end method match;

define method varsym? (x)
  instance?(x, <symbol>) & as(<string>, x)[0] == '?';
end method varsym?;

define method binding (x, binds)
  local method recbind (x, binds)
          aif(cl-assoc(x, binds), recbind(tail(it), binds) | it);
        end method recbind;
  let b = recbind(x, binds);
  values(tail(b), b);
end method binding;

// LTD: No macros.
#"if-match";

define method vars-in (expr,
                       #key atom?
                             = method (x)
                                 not(instance?(x, <list>));
                               end method)
  if (atom?(expr))
    if (var?(expr)) list(expr); end if;
  else
    union(vars-in(head(expr), atom?), vars-in(tail(expr), atom?));
  end if;
end method vars-in;

define method var? (x)
  instance?(x, <symbol>) & as(<string>, x)[0] == '?';
end method var?;

define method abab (seq)
  if-match(?x(?y, ?x, ?y), seq, values(?x, ?y), #f);
end method abab;

// LTD: No macros.
#"if-match";

// LTD: No macros.
#"pat-match";

define method simple? (x)
  not(instance?(x, <list>)) | head(x) == #"quote";
end method simple?;

define method gen-match (refs, then, else)
  if (empty?(refs))
    then;
  else
    let then = gen-match(tail(refs), then, else);
    if (simple?(head(head(refs))))
      match1(refs, then, else);
    else
      gen-match(head(refs), then, else);
    end if;
  end if;
end method gen-match;

define method match1 (refs, then, else)
  dbind((pat(expr))(rest), refs,
        if (gensym?(pat))
          list(#"let", list(list(pat, expr)),
               list(#"if",
                    list(#"and",
                         apply(list, #"typep", pat,
                               #(#(#"quote", #"sequence"))),
                         length-test(pat, rest)),
                    then, else));
        elseif (pat == #"_")
          then;
        elseif (var?(pat))
          begin
            let ge = generate-symbol();
            list(#"let", list(list(ge, expr)),
                 list(#"if",
                      list(#"or", list(#"gensym?", pat),
                           list(#"equal", pat, ge)),
                      list(#"let", list(list(pat, ge)), then), else));
          end;
        else
          list(#"if", list(#"equal", pat, expr), then, else);
        end if);
end method match1;

define method gensym? (s)
  instance?(s, <symbol>)
   & ~ // LTD: Function SYMBOL-PACKAGE not yet implemented.
       symbol-package(s);
end method gensym?;

define method length-test (pat, rest)
  let fin = caadar(copy-sequence(rest, start: size(rest) - 1));
  if (instance?(fin, <pair>) | fin == #"elt")
    list(#"=", list(#"length", pat), size(rest));
  else
    list(#">", list(#"length", pat), size(rest) - 2);
  end if;
end method length-test;

define method make-db (#key size = 100)
  make(<table>, size: size);
end method make-db;

define variable *default-db* = make-db();

define method clear-db (#key db = *default-db*)
  size(db) := 0;
end method clear-db;

// LTD: No macros.
#"db-query";

define method db-push (key, val, #key db = *default-db*)
  push!(val, db-query(key, db));
end method db-push;

// LTD: No macros.
#"fact";

// LTD: No macros.
#"with-answer";

define method interpret-query (expr, #key binds)
  select (car(expr))
    #"and"
       => interpret-and(reverse(tail(expr)), binds);
    #"or"
       => interpret-or(tail(expr), binds);
    #"not"
       => interpret-not(second(expr), binds);
    otherwise
       => lookup(head(expr), tail(expr), binds);
  end select;
end method interpret-query;

define method interpret-and (clauses, binds)
  if (empty?(clauses))
    list(binds);
  else
    apply(concatenate!,
          map(method (b) interpret-query(head(clauses), b); end method,
              interpret-and(tail(clauses), binds)));
  end if;
end method interpret-and;

define method interpret-or (clauses, binds)
  apply(concatenate!,
        map(method (c) interpret-query(c, binds); end method, clauses));
end method interpret-or;

define method interpret-not (clause, binds)
  if (interpret-query(clause, binds)) #f; else list(binds); end if;
end method interpret-not;

define method lookup (pred, args, #key binds)
  apply(concatenate!,
        map(method (x) aif2(match(x, args, binds), list(it)); end method,
            db-query(pred)));
end method lookup;

// LTD: No macros.
#"with-answer";

define method compile-query (q, body)
  select (car(q))
    #"and"
       => compile-and(tail(q), body);
    #"or"
       => compile-or(tail(q), body);
    #"not"
       => compile-not(second(q), body);
    #"lisp"
       => list(#"if", second(q), body);
    otherwise
       => compile-simple(q, body);
  end select;
end method compile-query;

define method compile-simple (q, body)
  let fact = generate-symbol();
  list(#"dolist", list(fact, list(#"db-query", list(#"quote", head(q)))),
       apply(list, #"pat-match", tail(q), fact, body, #(#())));
end method compile-simple;

define method compile-and (clauses, body)
  if (empty?(clauses))
    body;
  else
    compile-query(head(clauses), compile-and(tail(clauses), body));
  end if;
end method compile-and;

define method compile-or (clauses, body)
  if (empty?(clauses))
    #f;
  else
    let gbod = generate-symbol();
    let vars = vars-in(body, simple?);
    apply(list, #"labels", list(list(gbod, vars, body)),
          map(method (cl) compile-query(cl, pair(gbod, vars)); end method,
              clauses));
  end if;
end method compile-or;

define method compile-not (q, body)
  let tag = generate-symbol();
  list(#"if",
       apply(list, #"block", tag,
             compile-query(q, apply(list, #"return-from", tag, #(#()))),
             #(#"t")),
       body);
end method compile-not;

*cont* := identity;

// LTD: No macros.
#"=lambda";

// LTD: No macros.
#"=defun";

// LTD: No macros.
#"=bind";

// LTD: No macros.
#"=values";

// LTD: No macros.
#"=funcall";

// LTD: No macros.
#"=apply";

define variable *paths* = #f;

define constant failsym = #"@";

// LTD: No macros.
#"choose";

// LTD: No macros.
#"choose-bind";

define method cb (fn, choices)
  if (choices)
    if (tail(choices))
      push!(method () cb(fn, tail(choices)); end method, *paths*);
    end if;
    fn(head(choices));
  else
    fail();
  end if;
end method cb;

define method fail ()
  if (*paths*) (pop!(*paths*))(); else failsym; end if;
end method fail;

define class <proc> (<object>)
  slot proc-pri, init-keyword: #"proc-pri";
  slot proc-state, init-keyword: #"proc-state";
  slot proc-wait, init-keyword: #"proc-wait";
end class <proc>;

#f;

define variable *halt* = generate-symbol();

define variable *default-proc* =
  make-proc(state: method (x)
                     format-out("\n>> ");
                     print(// LTD: Function EVAL not yet implemented.
                           eval(// LTD: Function READ not yet implemented.
                                read()),
                           *standard-output*);
                     pick-process();
                   end method);

// LTD: No macros.
#"fork";

// LTD: No macros.
#"program";

define method pick-process ()
  let (p, val) = most-urgent-process();
  begin *proc* := p; *procs* := remove!(*procs*, p); end;
  (p.proc-state)(val);
end method pick-process;

define method most-urgent-process ()
  let proc1 = *default-proc*;
  let max = -1;
  let val1 = #t;
  for (p in *procs*)
    let pri = p.proc-pri;
    if (pri > max)
      let val = ~ p.proc-wait | (p.proc-wait)();
      if (val) begin proc1 := p; max := pri; val1 := val; end; end if;
    end if;
  end for;
  values(proc1, val1);
end method most-urgent-process;

define method arbitrator (test, cont)
  begin *proc*.proc-state := cont; *proc*.proc-wait := test; end;
  push!(*proc*, *procs*);
  pick-process();
end method arbitrator;

// LTD: No macros.
#"wait";

// LTD: No macros.
#"yield";

define method setpri (n) *proc*.proc-pri := n; end method setpri;

define method halt (#key val)
  // LTD: Can't convert a run-time catch tag.
  *halt*(val);
end method halt;

define method kill (#key obj, #rest args)
  if (obj)
    *procs* := apply(cl-remove!, obj, *procs*, args);
  else
    pick-process();
  end if;
end method kill;

define variable *open-doors* = #f;

=defun(pedestrian, #(),
       wait(d, head(*open-doors*), format-out("Entering %S\n", d)));

program(ped, #(), fork(pedestrian(), 1));

=defun(capture, city(), take(city), setpri(1), yield(fortify(city)));

=defun(plunder, city(), loot(city), ransom(city));

define method take (c) format-out("Liberating %S.\n", c); end method take;

define method fortify (c) format-out("Rebuilding %S.\n", c); end method fortify;

define method loot (c) format-out("Nationalizing %S.\n", c); end method loot;

define method ransom (c) format-out("Refinancing %S.\n", c); end method ransom;

program(barbarians, #(), fork(capture(#"rome"), 100),
        fork(plunder(#"rome"), 98));

// LTD: No macros.
#"defnode";

// LTD: No macros.
#"down";

// LTD: No macros.
#"cat";

// LTD: No macros.
#"jump";

define method compile-cmds (cmds)
  if (empty?(cmds))
    #"regs";
  else
    concatenate(head(cmds), list(compile-cmds(tail(cmds))));
  end if;
end method compile-cmds;

// LTD: No macros.
#"up";

// LTD: No macros.
#"getr";

// LTD: No macros.
#"set-register";

// LTD: No macros.
#"setr";

// LTD: No macros.
#"pushr";

// LTD: No macros.
#"with-parses";

define method types (word)
  select (word)
    (#"do", #"does", #"did")
       => #(#"aux", #"v");
    (#"time", #"times")
       => #(#"n", #"v");
    (#"fly", #"flies")
       => #(#"n", #"v");
    (#"like")
       => #(#"v", #"prep");
    (#"liked", #"likes")
       => #(#"v");
    (#"a", #"an", #"the")
       => #(#"det");
    (#"arrow", #"arrows")
       => #(#"n");
    (#"i", #"you", #"he", #"she", #"him", #"her", #"it")
       => #(#"pron");
    otherwise
       => #f;
  end select;
end method types;

defnode(mods, cat(n, mods/n, setr(mods, \*)));

defnode(mods/n, cat(n, mods/n, pushr(mods, \*)),
        up(list(#"n-group", getr(mods))));

defnode(np, cat(det, np/det, setr(det, \*)), jump(np/det, setr(det, #f)),
        cat(pron, pron, setr(n, \*)));

defnode(pron, up(list(#"np", list(#"pronoun", getr(n)))));

defnode(np/det, down(mods, np/mods, setr(mods, \*)),
        jump(np/mods, setr(mods, #f)));

defnode(np/mods, cat(n, np/n, setr(n, \*)));

defnode(np/n,
        up(list(#"np", list(#"det", getr(det)),
                list(#"modifiers", getr(mods)), list(#"noun", getr(n)))),
        down(pp, np/pp, setr(pp, \*)));

defnode(np/pp,
        up(list(#"np", list(#"det", getr(det)),
                list(#"modifiers", getr(mods)), list(#"noun", getr(n)),
                getr(pp))));

defnode(pp, cat(prep, pp/prep, setr(prep, \*)));

defnode(pp/prep, down(np, pp/np, setr(op, \*)));

defnode(pp/np,
        up(list(#"pp", list(#"prep", getr(prep)), list(#"obj", getr(op)))));

defnode(s, down(np, s/subj, setr(mood, #"decl"), setr(subj, \*)),
        cat(v, v, setr(mood, #"imp"),
            setr(subj, #(#"np", #(#"pron", #"you"))), setr(aux, #f),
            setr(v, \*)));

defnode(s/subj, cat(v, v, setr(aux, #f), setr(v, \*)));

defnode(v,
        up(list(#"s", list(#"mood", getr(mood)), list(#"subj", getr(subj)),
                list(#"vcl", list(#"aux", getr(aux)), list(#"v", getr(v))))),
        down(np, s/obj, setr(obj, \*)));

defnode(s/obj,
        up(list(#"s", list(#"mood", getr(mood)), list(#"subj", getr(subj)),
                list(#"vcl", list(#"aux", getr(aux)), list(#"v", getr(v))),
                list(#"obj", getr(obj)))));

// LTD: No macros.
#"with-inference";

define method rep_ (x)
  if (not(instance?(x, <list>)))
    if (x == #"_") generate-symbol(#"string"("?")); else x; end if;
  else
    pair(rep_(head(x)), rep_(tail(x)));
  end if;
end method rep_;

define method fullbind (x, b)
  if (varsym?(x))
    aif2(binding(x, b), fullbind(it, b), generate-symbol());
  elseif (not(instance?(x, <list>)))
    x;
  else
    pair(fullbind(head(x), b), fullbind(tail(x), b));
  end if;
end method fullbind;

define method varsym? (x)
  instance?(x, <symbol>) & as(<string>, x)[0] == '?';
end method varsym?;

// LTD: No macros.
#"with-inference";

define method varsym? (x)
  instance?(x, <symbol>)
   & ~ // LTD: Function SYMBOL-PACKAGE not yet implemented.
       symbol-package(x);
end method varsym?;

define method gen-query (expr, #key binds)
  select (car(expr))
    #"and"
       => gen-and(tail(expr), binds);
    #"or"
       => gen-or(tail(expr), binds);
    #"not"
       => gen-not(second(expr), binds);
    otherwise
       => list(#"prove",
               apply(list, #"list", list(#"quote", head(expr)),
                     map(form, tail(expr))),
               binds);
  end select;
end method gen-query;

define method gen-and (clauses, binds)
  if (empty?(clauses))
    list(#"=values", binds);
  else
    let gb = generate-symbol();
    list(#"=bind", list(gb), gen-query(head(clauses), binds),
         gen-and(tail(clauses), gb));
  end if;
end method gen-and;

define method gen-or (clauses, binds)
  pair(#"choose", map(method (c) gen-query(c, binds); end method, clauses));
end method gen-or;

define method gen-not (expr, binds)
  let gpaths = generate-symbol();
  list(#"let", list(pair(gpaths, #(#"*paths*"))), #(#"setq", #"*paths*", #()),
       list(#"choose",
            apply(list, #"=bind", #(#"b"), gen-query(expr, binds),
                  list(#"setq", #"*paths*", gpaths), #(#(#"fail"))),
            list(#"progn", list(#"setq", #"*paths*", gpaths),
                 list(#"=values", binds))));
end method gen-not;

=defun(prove, query(binds), choose-bind(r, *rules*, =funcall(r, query, binds)));

define method form (pat)
  if (simple?(pat))
    pat;
  else
    list(#"cons", form(head(pat)), form(tail(pat)));
  end if;
end method form;

define variable *rules* = #f;

// LTD: No macros.
#"<-";

define method rule-fn (ant, con)
  with-gensyms(val(win, fact, binds),
               list(#"=lambda", list(fact, binds),
                    list(#"with-gensyms", vars-in(list(ant, con), simple?),
                         list(#"multiple-value-bind", list(val, win),
                              list(#"match", fact,
                                   apply(list,
                                         #"list",
                                         list(#"quote", head(con)),
                                         map(form, tail(con))),
                                   binds),
                              apply(list, #"if", win, gen-query(ant, val),
                                    #(#(#"fail")))))));
end method rule-fn;

define method rule-fn (ant, con)
  with-gensyms(val(win, fact, binds, paths), // 
               apply(list, #"=lambda", list(fact, binds, paths),
                     #(// 
                       #(#"with-gensyms",
                         #(#(#","), #"vars-in", #(#"list", #"ant", #"con"),
                           #(#"function", #"simple?")),
                         #(#"multiple-value-bind",
                           #(#(#(#",") . #"val"), #(#(#",") . #"win")),
                           #(#"match", #(#(#",") . #"fact"),
                             #(#"list",
                               #(#"quote", #(#(#","), #"car", #"con")),
                               #(#(#",@"), #"mapcar", #(#"function", #"form"),
                                 #(#"cdr", #"con"))),
                             #(#(#",") . #"binds")),
                           #(#"if", #(#(#",") . #"win"),
                             #(#(#","), #"gen-query", #"ant", #"val",
                               #"paths"),
                             // 
                             #(#"fail")))))));
end method rule-fn;

// LTD: No macros.
#"with-inference";

define method gen-query (expr, binds, paths)
  // 
  select (car(expr))
    #"and"
       => gen-and(tail(expr), binds, paths);
    #"or"
       => gen-or(tail(expr), binds, paths);
    #"not"
       => gen-not(second(expr), binds, paths);
    #"lisp"
       => gen-lisp(second(expr), binds);
    #"is"
       => gen-is(second(expr), third(expr), binds);
    #"cut"
       => apply(list, #"progn", list(#"setq", #"*paths*", paths),
                #(// 
                  #(#"=values", #(#(#",") . #"binds"))));
    #"t"
       => apply(list, #"prove",
                apply(list, #"list", list(#"quote", head(expr)),
                      map(form, tail(expr))),
                binds, #(#"*paths*"));
  end select;
end method gen-query;

// 
=defun(prove, query(binds, paths), // 
       choose-bind(r, *rules*, =funcall(r, query, binds, paths)));

// 
define method gen-and (clauses, binds, paths)
  // 
  if (empty?(clauses))
    list(#"=values", binds);
  else
    let gb = generate-symbol();
    apply(list, #"=bind", list(gb), gen-query(head(clauses), binds, paths),
          #(// 
            #(#(#","), #"gen-and", #(#"cdr", #"clauses"), #"gb", #"paths")));
  end if;
end method gen-and;

// 
define method gen-or (clauses, binds, paths)
  // 
  pair(#"choose",
       map(method (c) gen-query(c, binds, paths); end method, // 
           clauses));
end method gen-or;

define method gen-not (expr, binds, paths)
  let gpaths = generate-symbol();
  list(#"let", list(pair(gpaths, #(#"*paths*"))), #(#"setq", #"*paths*", #()),
       list(#"choose",
            apply(list, #"=bind", #(#"b"), gen-query(expr, binds, paths),
                  #(// 
                    #(#"setq", #"*paths*", #(#(#",") . #"gpaths")),
                    #(#"fail"))),
            list(#"progn", list(#"setq", #"*paths*", gpaths),
                 list(#"=values", binds))));
end method gen-not;

// LTD: No macros.
#"with-binds";

define method gen-lisp (expr, binds)
  apply(list, #"if", list(#"with-binds", binds, expr),
        list(#"=values", binds), #(#(#"fail")));
end method gen-lisp;

define method gen-is (expr1, expr2, binds)
  apply(list, #"aif2",
        list(#"match", expr1, list(#"with-binds", binds, expr2), binds),
        #(#(#"=values", #"it"), #(#"fail")));
end method gen-is;

define method rget (obj, prop)
  some2(method (a) a[prop]; end method, get-ancestors(obj));
end method rget;

define method get-ancestors (obj)
  local method getall (x)
          concatenate(list(x),
                      apply(concatenate!, map(getall, x[#"parents"])));
        end method getall;
  sort!(cl-remove-duplicates!(getall(obj)), stable: #t,
        test: method (x, y) member?(y, x[#"parents"]); end method);
end method get-ancestors;

define method some2 (fn, lst)
  if (not(instance?(lst, <list>)))
    #f;
  else
    let (val, win) = fn(head(lst));
    if (val | win) values(val, win); else some2(fn, tail(lst)); end if;
  end if;
end method some2;

define method obj (#rest parents)
  let obj = make(<table>);
  obj[#"parents"] := parents;
  ancestors(obj);
  obj;
end method obj;

define method ancestors (obj)
  obj[#"ancestors"] | (obj[#"ancestors"] := get-ancestors(obj));
end method ancestors;

define method rget (obj, prop)
  some2(method (a) a[prop]; end method, ancestors(obj));
end method rget;

// LTD: No macros.
#"defprop";

define method run-methods (obj, name, args)
  let meth = rget(obj, name);
  if (meth)
    apply(meth, obj, args);
  else
    error("No %S method for %S.", name, obj);
  end if;
end method run-methods;

define class <meth> (<object>)
  slot meth-around, init-keyword: #"meth-around";
  slot meth-before, init-keyword: #"meth-before";
  slot meth-primary, init-keyword: #"meth-primary";
  slot meth-after, init-keyword: #"meth-after";
end class <meth>;

// LTD: No macros.
#"meth-";

define method run-methods (obj, name, args)
  let pri = rget(obj, name, #"primary");
  if (pri)
    let ar = rget(obj, name, #"around");
    if (ar)
      apply(ar, obj, args);
    else
      run-core-methods(obj, name, args, pri);
    end if;
  else
    error("No primary %S method for %S.", name, obj);
  end if;
end method run-methods;

define method run-core-methods (obj, name, args, #key pri)
  let (#rest _)
      = begin
          run-befores(obj, name, args);
          apply(pri | rget(obj, name, #"primary"), obj, args);
        end;
  run-afters(obj, name, args);
  apply(values, _);
end method run-core-methods;

define method rget (obj, prop, #key meth, skip = 0)
  some2(method (a)
          let (val, win) = a[prop];
          if (win)
            select (meth)
              #"around"
                 => meth-(around, val);
              #"primary"
                 => meth-(primary, val);
              otherwise
                 => values(val, win);
            end select;
          end if;
        end method,
        nth-tail(ancestors(obj), skip));
end method rget;

define method run-befores (obj, prop, args)
  for (a in ancestors(obj))
    let bm = meth-(before, a[prop]);
    if (bm) apply(bm, obj, args); end if;
  end for;
end method run-befores;

define method run-afters (obj, prop, args)
  local method rec (lst)
          if (lst)
            rec(tail(lst));
            let am = meth-(after, head(lst)[prop]);
            if (am) apply(am, head(lst), args); end if;
          end if;
        end method rec;
  rec(ancestors(obj));
end method run-afters;

// LTD: No macros.
#"defmeth";

define method build-meth (name, type, gobj, parms, body)
  let gargs = generate-symbol();
  list(#"function",
       list(#"lambda", list(#"&rest", gargs),
            list(#"labels",
                 list(list(#"call-next", #"()",
                           if (type == #"primary" | type == #"around")
                             list(#"cnm", gobj, list(#"quote", name),
                                  list(#"cdr", gargs), type);
                           else
                             #(#"error", "Illegal call-next.");
                           end if),
                      list(#"next-p", #"()",
                           select (type)
                             #"around"
                                => list(#"or",
                                        apply(list,
                                              #"rget",
                                              gobj,
                                              list(#"quote", name),
                                              #(#"around", 1)),
                                        apply(list,
                                              #"rget",
                                              gobj,
                                              list(#"quote", name),
                                              #(#"primary")));
                             #"primary"
                                => apply(list,
                                         #"rget",
                                         gobj,
                                         list(#"quote", name),
                                         #(#"primary", 1));
                             otherwise
                                => #f;
                           end select)),
                 list(#"apply",
                      list(#"function", apply(list, #"lambda", parms, body)),
                      gargs))));
end method build-meth;

define method cnm (obj, name, args, type)
  select (type)
    #"around"
       => let ar = rget(obj, name, around: 1);
           if (ar)
             apply(ar, obj, args);
           else
             run-core-methods(obj, name, args);
           end if;
    #"primary"
       => let pri = rget(obj, name, primary: 1);
           if (pri)
             apply(pri, obj, args);
           else
             error("No next method.");
           end if;
    otherwise
       => #f;
  end select;
end method cnm;

// LTD: No macros.
#"undefmeth";

// LTD: No macros.
#"children";

define method parents (obj) obj[#"parents"]; end method parents;

define method set-parents (obj, pars)
  for (p in parents(obj)) children(p) := remove!(children(p), obj); end for;
  obj[#"parents"] := pars;
  for (p in pars)
    let new-value-141276 = obj;
    let g141275 = p;
    let g141274 = add!(new-value-141276, children(g141275));
    "common-lisp-user" "children"(g141274, g141275);
  end for;
  maphier(method (obj) obj[#"ancestors"] := get-ancestors(obj); end method,
          obj);
  pars;
end method set-parents;

// LTD: No setf macros.
#"parents";

define method maphier (fn, obj)
  fn(obj);
  for (c in children(obj)) maphier(fn, c); end for;
end method maphier;

define method obj (#rest parents)
  let obj = make(<table>);
  parents(obj) := parents;
  obj;
end method obj;

// LTD: No macros.
#"defcomb";

define method run-core-methods (obj, name, args, #key pri)
  let comb = symbol-get-property(name, #"mcombine");
  if (comb)
    if (instance?(comb, <symbol>))
      (select (comb)
         #"and"
            => comb-and;
         #"or"
            => comb-or;
         otherwise
            => #f;
       end select)(obj, name, args, ancestors(obj));
    else
      comb-normal(comb, obj, name, args);
    end if;
  else
    let (#rest _)
        = begin
            run-befores(obj, name, args);
            apply(pri | rget(obj, name, #"primary"), obj, args);
          end;
    run-afters(obj, name, args);
    apply(values, _);
  end if;
end method run-core-methods;

define method comb-normal (comb, obj, name, args)
  apply(comb,
        apply(concatenate!,
              map(method (a)
                    let pm = meth-(primary, a[name]);
                    let val = if (pm) apply(pm, obj, args); end if;
                    if (val) list(val); end if;
                  end method,
                  ancestors(obj))));
end method comb-normal;

define method comb-and (obj, name, args, ancs, #key last = #t)
  if (empty?(ancs))
    last;
  else
    let pm = meth-(primary, head(ancs)[name]);
    if (pm)
      let new = apply(pm, obj, args);
      new & comb-and(obj, name, args, tail(ancs), new);
    else
      comb-and(obj, name, args, tail(ancs), last);
    end if;
  end if;
end method comb-and;

define method comb-or (obj, name, args, ancs)
  ancs
   & begin
       let pm = meth-(primary, head(ancs)[name]);
       (pm & apply(pm, obj, args) | comb-or(obj, name, args, tail(ancs)));
     end;
end method comb-or;

// LTD: No macros.
#"undefmethod";

define method udm (name, qual, specs)
  let classes
      = map(method (s) list(#"find-class", list(#"quote", s)); end method,
            specs);
  list(#"remove-method", list(#"symbol-function", list(#"quote", name)),
       list(#"find-method", list(#"symbol-function", list(#"quote", name)),
            list(#"quote", qual), pair(#"list", classes)));
end method udm;

define method compall ()
  let g141605 = packagify(*package*);
  let g141611 = #"ext";
  let g141612 = %package-internal-symbols(g141605);
  let g141610 :: <list> = %package-shadowing-symbols(g141605);
  let s = #f;
  block (return)
    while (#t)
      let g141606 = #f;
      let g141607 = #f;
      let g141608 = #f;
      local method go-g141613 () #f; end method go-g141613,
            method go-g141614 ()
              if (g141606 >= g141608) go-g141613(); end if;
              s := g141607[g141606];
              if (instance?(s, <symbol>))
                let g141609
                    = member?(symbol-name$symbol(s), g141610,
                              test: method (x, y)
                                      x = symbol-name$symbol(y);
                                    end method);
                if (~ (g141609 & ~ (head(g141609) == s)))
                  if (// LTD: Function FBOUNDP not yet implemented.
                      fboundp(s))
                    if (~ instance?(s, <function>))
                      print(s, *standard-output*);
                      // LTD: Function COMPILE not yet implemented.
                      compile(s);
                    end if;
                  end if;
                end if;
              end if;
              g141606 := g141606 + 1;
              go-g141614();
              go-g141613();
            end method go-g141614;
      g141606 := 0;
      g141607 := g141612;
      g141608 := vector-size$vector(g141607);
      go-g141614();
      if (g141611)
        if (g141611 == #"ext")
          g141612 := %package-external-symbols(g141605);
          g141611 := %package-use-list(g141605);
        else
          g141612 := %package-external-symbols(head(g141611));
          g141611 := tail(g141611);
        end if;
      else
        s := #f;
        return(#f);
      end if;
    end while;
  end block;
end method compall;

// LTD: No macros.
#"check";

//  This code is copyright 1993 by Paul Graham, but anyone who wants 
//  to use the code in any nonprofit activity, or distribute free
//  verbatim copies (including this notice), is encouraged to do so.
"eof";

