//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Backtrack.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method backtrack (conjunction)
  // No answers were found, so back up to where a relevant variable was bound
  compute-nogoods(conjunction);
  backjump(conjunction);
  maybe-expand-conjunction(conjunction);
end method backtrack;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
define method compute-nogoods (conjunction)
  // Record the index of previous conjuncts that bound variables in failed one
  block (return-from-compute-nogoods)
    if (~ *use-backjumping*) return-from-compute-nogoods(#f); end if;
    if (conjunction.stack-pointer = conjunction.backtrack-pointer)
      return-from-compute-nogoods(#f);
    end if;
    let g151404 = active-conjunct(conjunction);
    begin
      if (g151404.nogoods == #"uninitialized") g151404.nogoods := #f; end if;
      begin
        let failed-vars = literal-vars-in(g151404.literal);
        for (conjunct-index from 0, answer in reverse(conjunction.stack))
          if (any?(method (var) answer-binds-var-p(answer, var); end method,
                   failed-vars))
            let new-value-151406 = conjunct-index;
            let g151405 = add!(new-value-151406, g151404.nogoods, test: \=);
            set-slot-value(g151404, #"nogoods", g151405);
          end if;
        end for;
      end;
    end;
  end block;
end method compute-nogoods;

// ----------------------------------------------------------------------------
define method backjump (conjunction)
  // Back up conjunction to most recent nogood justification
  let nogoods = conjunction.list[conjunction.stack-pointer].nogoods;
  if (~ *use-backjumping*
       | conjunction.stack-pointer = conjunction.backtrack-pointer)
    if (*use-backjumping*) dec!(conjunction.backtrack-pointer); end if;
    //  Chronological backtracking
    if (stack-pointer = 0)
      nogoods := #f;
    else
      nogoods := list(stack-pointer - 1);
    end if;
  end if;
  if (instance?(conjunction, <dtp-forked-conjunction>)
       & conjunction.stack-pointer <= conjunction.top-conjunct)
    nogoods := #f;
  end if;
  if (nogoods)
    let new-index = max(reduce1(max, nogoods), conjunction.backtrack-pointer);
    for (index from conjunction.stack-pointer to new-index + 1,
         conjunct = conjunction.list[index] then conjunction.list[index],
         subgoal = conjunct.subgoal then conjunct.subgoal)
      if (instance?(subgoal, <dtp-subgoal>))
        subgoal.conjuncts-to-propagate-to
         := remove(subgoal.conjuncts-to-propagate-to, conjunct);
      end if;
      reset(conjunct);
      pop!(conjunction.stack);
      dec!(conjunction.stack-pointer);
    finally
      conjunct := conjunction.list[new-index];
      conjunct.nogoods
       := union(if (~ (conjunct.nogoods == #"uninitialized"))
                  conjunct.nogoods;
                end if,
                remove(nogoods,
                       complement(method (n) n < new-index; end method)),
                test: \=);
      #f;
    end for;
  else
    for (conjunct in conjunction.list) reset(conjunct); end for;
    conjunction.stack := #f;
    conjunction.stack-pointer := -1;
  end if;
end method backjump;

// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
nil(#f, nil(), "Unless waiting for next subgoal answer, get the next answer",
    nil((nil(nil(#f)))(),
        nil(#f,
            nil(nil(), #f,
                nil(nil(nil(#f, #()), nil(#f, nil(#f, #()))), nil(#f))))));

// ----------------------------------------------------------------------------
"eof";

