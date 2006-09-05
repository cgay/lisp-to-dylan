Module:    CL-internals
Author:    Scott McKay, Peter Norvig
Copyright: 1995 by Harlequin, Inc.

// Returns the index at which ITEM was found
define generic cl-position (sequence :: <sequence>, item,
			    #key test, key, start, end: finish, from-end?)
 => index :: false-or(<integer>);

define method cl-position
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish, start, -1)	//--- fencepost errors when from-end
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      let tkey = if (key) key(telt) else telt end;
      when (test(item, tkey))
        return(i)
      end;
    finally #f;
    end
  end
end method cl-position;


// Returns the index at which PREDICATE returned true
define generic cl-position-if (sequence :: <sequence>, predicate,
			       #key key, start, end: finish, from-end?)
 => index :: false-or(<integer>);

define method cl-position-if
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish, start, -1)	//--- fencepost errors when from-end
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      let tkey = if (key) key(telt) else telt end;
      when (predicate(tkey))
        return(i)
      end;
    finally #f;
    end
  end
end method cl-position-if;


// Returns the whole value in which ITEM was found
define generic cl-find (sequence :: <sequence>, item,
			#key test, key, start, end: finish, from-end?)
 => item :: <object>;

define method cl-find
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?) => item :: <object>;
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish, start, -1)	//--- fencepost errors when from-end
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      let tkey = if (key) key(telt) else telt end;
      when (test(item, tkey))
        return(telt)
      end;
    finally #f;
    end
  end
end method cl-find;


// Returns the whole value for which PREDICATE returned true
define generic cl-find-if (sequence :: <sequence>, predicate,
			   #key key, start, end: finish, from-end?)
 => item :: <object>;

define method cl-find-if
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?) => item :: <object>;
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish, start, -1)	//--- fencepost errors when from-end
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      let tkey = if (key) key(telt) else telt end;
      when (predicate(tkey))
        return(telt)
      end;
    finally #f;
    end
  end
end method cl-find-if;


// Like FIND, but the sequence is a CL-style alist
define generic cl-assoc (sequence :: <sequence>, item,
			 #key test, key, start, end: finish, from-end?)
 => pair :: <object>;

define method cl-assoc
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?) => pair :: <object>;
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish, start, -1)	//--- fencepost errors when from-end
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      when (telt)
        // skip null items
        let tkey = if (key) key(telt[0]) else telt[0] end;
        when (test(item, tkey))
          return(telt)
        end
      end;
    finally #f;
    end
  end
end method cl-assoc;


// Ditto
define generic cl-assoc-if (sequence :: <sequence>, item,
			    #key key, start, end: finish, from-end?)
 => pair :: <object>;

define method cl-assoc-if
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?) => pair :: <object>;
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish, start, -1)	//--- fencepost errors when from-end
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      when (telt)
        // skip null items
        let tkey = if (key) key(head(telt)) else head(telt) end;
        when (predicate(tkey))
          return(telt)
        end
      end;
    finally #f;
    end
  end
end method cl-assoc-if;


// Counts each occurrence of ITEM in the sequence
define generic cl-count (sequence :: <sequence>, item,
			 #key test, key, start, end: finish, from-end?)
 => count :: <integer>;

define method cl-count
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?) => count :: <integer>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let (start, finish, increment)
    = if (from-end?)
        values(finish, start, -1)	//--- fencepost errors when from-end
      else
        values(start, finish, 1)
      end;
  let n = 0;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    when (test(item, tkey))
      n := n + 1
    end;
  finally n;
  end
end method cl-count;


// Counts each occurrence in the sequence for which PREDICATE returned true
define generic cl-count-if (sequence :: <sequence>, item,
			    #key key, start, end: finish, from-end?)
 => count :: <integer>;

define method cl-count-if
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?) => count :: <integer>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let (start, finish, increment)
    = if (from-end?)
        values(finish, start, -1)	//--- fencepost errors when from-end
      else
        values(start, finish, 1)
      end;
  let n = 0;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    when (predicate(tkey))
      n := n + 1
    end;
  finally n;
  end
end method cl-count-if;


// Like CL REMOVE
define generic cl-remove (sequence :: <sequence>, item,
			  #key test, key, start, end: finish, from-end?, count)
 => result :: <sequence>;

define method cl-remove
    (sequence :: <sequence>, item, #rest keys,
     #key test = \==, key, start = 0, end: finish, from-end?, count) => result :: <sequence>;
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?, count;
  apply(cl-remove!, copy-sequence(sequence), item, keys)
end method cl-remove;


// Like CL REMOVE-IF
define generic cl-remove-if (sequence :: <sequence>, item,
			     #key key, start, end: finish, from-end?, count)
 => result :: <sequence>;

define method cl-remove-if
    (sequence :: <sequence>, predicate, #rest keys,
     #key key, start = 0, end: finish, from-end?, count) => result :: <sequence>;
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?, count;
  apply(cl-remove-if!, copy-sequence(sequence), predicate, keys)
end method cl-remove-if;


// Like CL DELETE
define generic cl-remove! (sequence :: <sequence>, item,
			   #key test, key, start, end: finish, from-end?, count)
 => result :: <sequence>;

define method cl-remove!
    (sequence :: <list>, item,
     #key test = \==, key, start = 0, end: finish, from-end?, count) => result :: <list>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-remove!;

define method cl-remove!
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?, count) => result :: <sequence>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let output-index = start;
  let (start, finish, increment)
    = if (from-end?)
        values(finish, start, -1)
      else
        values(start, finish, 1)
      end;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    unless (test(item, tkey))
      when (~count
            | begin
                let _value = positive?(count);
                count := count - 1;
                _value
              end)
        //--- not correct
        sequence[output-index] := telt;
        output-index := output-index + 1
      end
    end;
  finally
    begin
      size(sequence) := output-index;
      sequence
    end;
  end
end method cl-remove!;


// Like CL DELETE-IF
define generic cl-remove-if! (sequence :: <sequence>, item,
			      #key key, start, end: finish, from-end?, count)
 => result :: <sequence>;

define method cl-remove-if!
    (sequence :: <list>, predicate,
     #key key, start = 0, end: finish, from-end?, count) => result :: <list>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-remove-if!;

define method cl-remove-if!
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?, count) => result :: <sequence>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let output-index = start;
  let (start, finish, increment)
    = if (from-end?)
        values(finish, start, -1)
      else
        values(start, finish, 1)
      end;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    unless (predicate(tkey))
      when (~count
            | begin
                let _value = positive?(count);
                count := count - 1;
                _value
              end)
        //--- not correct
        sequence[output-index] := telt;
        output-index := output-index + 1
      end
    end;
  finally
    begin
      size(sequence) := output-index;
      sequence
    end;
  end
end method cl-remove-if!;


define generic cl-substitute (sequence :: <sequence>, newitem, olditem,
			      #key test, key, start, end: finish, from-end?, count)
 => result :: <sequence>;

define method cl-substitute
    (sequence :: <sequence>, newitem, olditem, #rest keys,
     #key test = \==, key, start = 0, end: finish, from-end?, count) => result :: <sequence>;
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?, count;
  apply(cl-substitute!, copy-sequence(sequence), newitem, olditem, keys)
end method cl-substitute;


define generic cl-substitute-if (sequence :: <sequence>, newitem, predicate,
				 #key key, start, end: finish, from-end?, count)
 => result :: <sequence>;

define method cl-substitute-if
    (sequence :: <sequence>, newitem, predicate, #rest keys,
     #key key, start = 0, end: finish, from-end?, count) => result :: <sequence>;
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?, count;
  apply(cl-substitute-if!, copy-sequence(sequence), newitem, predicate, keys)
end method cl-substitute-if;


define generic cl-substitute! (sequence :: <sequence>, newitem, olditem,
			      #key test, key, start, end: finish, from-end?, count)
 => result :: <sequence>;

define method cl-substitute!
    (sequence :: <list>, newitem, olditem,
     #key test = \==, key, start = 0, end: finish, from-end?, count) => result :: <list>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-substitute!;

define method cl-substitute!
    (sequence :: <sequence>, newitem, olditem,
     #key test = \==, key, start = 0, end: finish, from-end?, count) => result :: <sequence>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let (start, finish, increment)
    = if (from-end?)
        values(finish, start, -1)
      else
        values(start, finish, 1)
      end;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    when (test(olditem, tkey))
      when (~count
            | begin
                let _value = positive?(count);
                count := count - 1;
                _value
              end)
        sequence[i] := newitem
      end
    end;
  finally sequence;
  end
end method cl-substitute!;


define generic cl-substitute-if! (sequence :: <sequence>, newitem, predicate,
				  #key key, start, end: finish, from-end?, count)
 => result :: <sequence>;

define method cl-substitute-if!
    (sequence :: <list>, newitem, predicate,
     #key key, start = 0, end: finish, from-end?, count) => result :: <list>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-substitute-if!;

define method cl-substitute-if!
    (sequence :: <sequence>, newitem, predicate,
     #key key, start = 0, end: finish, from-end?, count) => result :: <sequence>;
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let (start, finish, increment)
    = if (from-end?)
        values(finish, start, -1)
      else
        values(start, finish, 1)
      end;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    when (predicate(tkey))
      when (~count
            | begin
                let _value = positive?(count);
                count := count - 1;
                _value
              end)
        sequence[i] := newitem
      end
    end;
  finally sequence;
  end
end method cl-substitute-if!;


// Like CL REMOVE-DUPLICATES
define method cl-remove-duplicates
    (sequence :: <sequence>, #rest keys,
     #key test = \==, key, start = 0, end: finish, from-end?)
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?;
  apply(cl-remove-duplicates!, copy-sequence(sequence), keys)
end method cl-remove-duplicates;

// Like CL DELETE-DUPLICATES
define method cl-remove-duplicates!
    (sequence :: <list>, #key test = \==, key, start = 0, end: finish, from-end?)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-remove-duplicates!;

define method cl-remove-duplicates!
    (sequence :: <sequence>,
     #key test = \==, key, start = 0, end: finish, from-end?, replace?)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  begin
    let result-index = start;
    for (index from start below size(sequence))
      let test-element = sequence[index];
      let test-key = if (key) key(test-element) else test-element end;
      case
        index >= finish
        | if (from-end?)
            //never duplicated
            block (return)
              for (tindex from start below result-index)
                let elt = sequence[tindex];
                let tkey = if (key) key(elt) else elt end;
                when (test(tkey, test-key))
                  // TEST-ELEMENT is an earlier duplicate of element
                  when (replace?)
                    sequence[tindex] := test-element
                  end;
                  return(#f)
                end;
              finally
                #t;
              end
            end block
          else
            block (return)
              for (tindex from index + 1 below finish)
                let elt = sequence[tindex];
                let tkey = if (key) key(elt) else elt end;
                when (test(test-key, tkey))
                  return(#f)
                end;
              finally
                #t;
              end
            end block
          end =>
          // Not a duplicate
          sequence[result-index] := test-element;
          result-index := result-index + 1
      end;
    finally
      size(sequence) := result-index;
    end
  end;
  sequence
end method cl-remove-duplicates!;


define generic cl-search (sequence1 :: <sequence>, sequence2 :: <sequence>,
			  #key test, key, start1, start2, end1, end2, from-end?)
 => index :: false-or(<integer>);

define method cl-search
    (sequence1 :: <list>, sequence2 :: <list>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    when (positive?(start1))
      for (i from 0 below start1)
        pop!(sequence1);
      end;
      end1 := end1 - start1;
      start1 := 0
    end;
    when (positive?(start2))
      for (i from 0 below start2)
        pop!(sequence2);
      end
    end;
    when (from-end?)
      return
        (cl-reverse-search
           (sequence1, sequence2, start1, start2, end1, end2, test, key))
    end;
    when (empty?(sequence1) | (end1 & start1 >= end1))
      return(start2)
    end;
    let telt1 = head(sequence1);
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2,
         until: (end2 & i = end2) | empty?(sequence2))
      let telt2 = head(sequence2);
      let rest2 = tail(sequence2);
      let tkey2 = if (key) key(telt2) else telt2 end;
      pop!(sequence2);
      when (test(tkey1, tkey2))
        when (block (break)
                for (i2 from i + 1, i1 from start1 + 1,
                     telt1 in tail(sequence1),
                     telt2 = pop!(rest2) then pop!(rest2),
                     until: ((end2 & i2 = end2) | empty?(rest2)) & return(#f))
                  when (key)
                    telt1 := key(telt1);
                    telt2 := key(telt2)
                  end;
                  unless (test(telt1, telt2))
                    break(#f)
                  end;
                finally break(#t);
                end
              end)
          return(i)
        end
      end;
    end
  end
end method cl-search;

define method cl-reverse-search
    (sequence1 :: <list>, sequence2 :: <list>,
     start1, start2, end1, end2, test, key) => index :: false-or(<integer>);
  block (return)
    let len1 = end1 - start1;
    let len2 = end2 - start2;
    when (len1 > len2)
      return(#f)
    end;
    unless (len1 > 0)
      return(end2)
    end;
    let last-found = #f;
    let telt1 = head(sequence1);
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2 to end2 - len1)
      let telt2 = head(sequence2);
      let rest2 = tail(sequence2);
      let tkey2 = if (key) key(telt2) else telt2 end;
      pop!(sequence2);
      when (test(tkey1, tkey2))
        when (block (break)
                for (i1 from start1 + 1,
		     telt1 in tail(sequence1),
                     telt2 = pop!(rest2) then pop!(rest2),
		     until: i1 >= end1)
                  when (key)
                    telt1 := key(telt1);
                    telt2 := key(telt2)
                  end;
                  unless (test(telt1, telt2))
                    break(#f)
                  end;
                finally break(#t);
                end
              end)
          last-found := i
        end
      end;
    finally return(last-found);
    end
  end
end method cl-reverse-search;

define method cl-search
    (sequence1 :: <list>, sequence2 :: <sequence>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    when (positive?(start1))
      for (i from 0 below start1)
        pop!(sequence1);
      end;
      end1 := end1 - start1;
      start1 := 0
    end;
    when (from-end?)
      return
        (cl-reverse-search
           (sequence1, sequence2, start1, start2, end1, end2, test, key))
    end;
    when (empty?(sequence1) | (end1 & start1 >= end1))
      return(start2)
    end;
    let telt1 = head(sequence1);
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2 below end2)
      let telt2 = sequence2[i];
      let tkey2 = if (key) key(telt2) else telt2 end;
      when (test(tkey1, tkey2))
        when (block (break)
                for (i2 from i + 1, i1 from start1 + 1,
                     telt1 in tail(sequence1),
                     until: i2 = end2 & return(#f))
		  let telt2 = sequence2[i2];
                  when (key)
                    telt1 := key(telt1);
                    telt2 := key(telt2)
                  end;
                  unless (test(telt1, telt2))
                    break(#f)
                  end;
                finally break(#t);
                end
              end)
          return(i)
        end
      end;
    end
  end
end method cl-search;

define method cl-reverse-search
    (sequence1 :: <list>, sequence2 :: <sequence>,
     start1, start2, end1, end2, test, key) => index :: false-or(<integer>);
  block (return)
    let len1 = end1 - start1;
    let len2 = end2 - start2;
    when (len1 > len2)
      return(#f)
    end;
    unless (len1 > 0)
      return(end2)
    end;
    let telt1 = head(sequence1);
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from end2 - len1 to start2 by -1)
      let telt2 = sequence2[i];
      let tkey2 = if (key) key(telt2) else telt2 end;
      when (test(tkey1, tkey2))
        when (block (break)
                for (i2 from i + 1, i1 from start1 + 1,
                     telt1 in tail(sequence1),
                     until: i1 >= end1)
		  let telt2 = sequence2[i2];
                  when (key)
                    telt1 := key(telt1);
                    telt2 := key(telt2)
                  end;
                  unless (test(telt1, telt2))
                    break(#f)
                  end;
                finally break(#t);
                end
              end)
          return(i)
        end
      end;
    end
  end
end method cl-reverse-search;

define method cl-search
    (sequence1 :: <sequence>, sequence2 :: <list>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    when (positive?(start2))
      for (i from 0 below start2)
        pop!(sequence2);
      end
    end;
    when (from-end?)
      return
        (cl-reverse-search
           (sequence1, sequence2, start1, start2, end1, end2, test, key))
    end;
    when (empty?(sequence1) | (end1 & start1 >= end1))
      return(start2)
    end;
    let telt1 = sequence1[start1];
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2, 
	 until: (end2 & i = end2) | empty?(sequence2))
      let telt2 = head(sequence2);
      let rest2 = tail(sequence2);
      let tkey2 = if (key) key(telt2) else telt2 end;
      pop!(sequence2);
      when (test(tkey1, tkey2))
        when (block (break)
                for (i2 from i + 1, i1 from start1 + 1 below end1,
                     until: ((end2 & i2 = end2) | empty?(rest2)) & return(#f))
		  let telt1 = sequence1[i1];
		  let telt2 = pop!(rest2);
		  when (key)
                    telt1 := key(telt1);
                    telt2 := key(telt2)
                  end;
                  unless (test(telt1, telt2))
                    break(#f)
                  end;
                finally break(#t);
                end
              end)
          return(i)
        end
      end;
    end
  end
end method cl-search;

define method cl-reverse-search
    (sequence1 :: <sequence>, sequence2 :: <list>,
     start1, start2, end1, end2, test, key) => index :: false-or(<integer>);
  block (return)
    let len1 = end1 - start1;
    let len2 = end2 - start2;
    when (len1 > len2)
      return(#f)
    end;
    unless (len1 > 0)
      return(end2)
    end;
    let last-found = #f;
    let telt1 = sequence1[start1];
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2 to end2 - len1)
      let telt2 = head(sequence2);
      let rest2 = tail(sequence2);
      let tkey2 = if (key) key(telt2) else telt2 end;
      pop!(sequence2);
      when (test(tkey1, tkey2))
        when (block (break)
                for (i1 from start1 + 1 below end1)
		  let telt1 = sequence1[i1];
		  let telt2 = pop!(rest2);
                  when (key)
                    telt1 := key(telt1);
                    telt2 := key(telt2)
                  end;
                  unless (test(telt1, telt2))
                    break(#f)
                  end;
                finally break(#t);
                end
              end)
          last-found := i
        end
      end;
    finally return(last-found);
    end
  end
end method cl-reverse-search;

define method cl-search
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    when (from-end?)
      return
        (cl-reverse-search
           (sequence1, sequence2, start1, start2, end1, end2, test, key))
    end;
    when (empty?(sequence1) | (end1 & start1 >= end1))
      return(start2)
    end;
    let telt1 = sequence1[start1];
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2 below end2)
      let telt2 = sequence2[i];
      let tkey2 = if (key) key(telt2) else telt2 end;
      when (test(tkey1, tkey2))
        when (block (break)
                for (i2 from i + 1, i1 from start1 + 1 below end1,
                     until: i2 = end2 & return(#f))
                  let telt1 = sequence1[i1];
                  let telt2 = sequence2[i2];
                  when (key)
                    telt1 := key(telt1);
                    telt2 := key(telt2)
                  end;
                  unless (test(telt1, telt2))
                    break(#f)
                  end;
                finally break(#t);
                end
              end)
          return(i)
        end
      end;
    end
  end
end method cl-search;

define method cl-reverse-search
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     start1, start2, end1, end2, test, key) => index :: false-or(<integer>);
  block (return)
    let len1 = end1 - start1;
    let len2 = end2 - start2;
    when (len1 > len2)
      return(#f)
    end;
    unless (len1 > 0)
      return(end2)
    end;
    let telt1 = sequence1[start1];
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from end2 - len1 to start2 by -1)
      let telt2 = sequence2[i];
      let tkey2 = if (key) key(telt2) else telt2 end;
      when (test(tkey1, tkey2))
        when (block (break)
                for (i2 from i + 1, i1 from start1 + 1 below end1)
		  let telt1 = sequence1[i1];
		  let telt2 = sequence2[i2];
                  when (key)
                    begin
                      telt1 := key(telt1);
                      telt2 := key(telt2)
                    end
                  end;
                  unless (test(telt1, telt2))
                    break(#f)
                  end;
                finally break(#t);
                end
              end)
          return(i)
        end
      end;
    end
  end
end method cl-reverse-search;

/*|
cl-search(#(1,2,5,4), #(1,2,3,4,5), start1: 2, end1: 4); ==> #f
cl-search(#(1,2,5,4), #(1,2,3,4,5), start1: 2, end1: 5); ==> fail
cl-search(#(4), #(1,2,3,4,5), start2: 2, end2: 5); ==> 3

cl-search(#(3,4,5), #(1,2,3,4,5), start1: 1, end1: 2, start2: 2); ==> 3
cl-search(#(3,4,5), #(1,2,3,4,5), start1: 1, end1: 2, start2: 1); ==> 3
cl-search(#[3,4,5], #(1,2,3,4,5,2,3,4,5,6), start1: 2, end1: 2); => 0
cl-search(#[3,4,5], #(1,2,3,4,5,2,3,4,5,6), start1: 2, end1: 3); => 4

cl-search(#(1,2,5,4), #(1,2,3,4,5), start1: 2, end1: 4, from-end?: #t); ==> #f
cl-search(#(1,2,5,4), #(1,2,3,4,5), start1: 2, end1: 5, from-end?: #t); ==> fail
cl-search(#(4), #(1,2,3,4,5), start2: 2, end2: 5, from-end?: #t); ==> 3

cl-search(#(3,4,5), #(1,2,3,4,5), start1: 1, end1: 2, start2: 2, from-end?: #t); ==> 3
cl-search(#(3,4,5), #(1,2,3,4,5), start1: 1, end1: 2, start2: 1, from-end?: #t); ==> 3
cl-search(#[3,4,5], #(1,2,3,4,5,2,3,4,5,6), start1: 2, end1: 2, from-end?: #t); => 10
cl-search(#[3,4,5], #(1,2,3,4,5,2,3,4,5,6), start1: 2, end1: 3, from-end?: #t); => 8
cl-search(#(1), #(1)); => 0
cl-search("", "abcde"); => 0
cl-search("abcde", "abcde", start1: 3, end1: 3); => 0
cl-search(#(1), #(1), from-end?: #t); => 0
cl-search("", "abcde", from-end?: #t); => 5
cl-search("abcde", "abcde", start1: 3, end1: 3, from-end?: #t); => 5
cl-search(#(1), #(1), end1: 0); => 0
cl-search(#(1), #(1), end1: 0, from-end?: #t); => 1
|*/


define generic cl-mismatch (sequence1 :: <sequence>, sequence2 :: <sequence>,
			    #key test, key, start1, start2, end1, end2, from-end?)
 => index :: false-or(<integer>);

define method cl-mismatch
    (sequence1 :: <list>, sequence2 :: <list>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    when (positive?(start1))
      for (i from 0 below start1)
        pop!(sequence1);
      end
    end;
    when (positive?(start2))
      for (i from 0 below start2)
        pop!(sequence2);
      end
    end;
    let last-difference = #f;
    case
      from-end? =>
        unless (end1)
          end1 := start1 + size(sequence1)
        end;
        unless (end2)
          end2 := start2 + size(sequence2)
        end;
        begin
          let len1 = end1 - start1;
          let len2 = end2 - start2;
          case
            len1 > len2 =>
              let diff = len1 - len2;
	      start1 := start1 + diff;
              last-difference := start1;
              for (i from 0 below diff)
                pop!(sequence1);
              end;
            len2 > len1 =>
              let diff = len2 - len1;
	      start2 := start2 + diff;
              last-difference := start1;
              for (i from 0 below diff)
                pop!(sequence2);
              end
          end
        end;
        until (start1 = end1)
          begin
            let elt1 = pop!(sequence1);
            let elt2 = pop!(sequence2);
            when (key)
              elt1 := key(elt1);
              elt2 := key(elt2)
            end;
            unless (test(elt1, elt2))
              last-difference := start1 + 1
            end
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end;
        last-difference;
      otherwise =>
        while (#t)
          case
            empty?(sequence1) | (end1 & start1 = end1) =>
              return
                (if (empty?(sequence2) | (end2 & start2 = end2))
                   #f
                 else
                   start1
                 end);
            empty?(sequence2) | (end2 & start2 = end2) =>
              return(start1)
          end;
          let elt1 = pop!(sequence1);
          let elt2 = pop!(sequence2);
          when (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(start1)
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end
    end
  end
end method cl-mismatch;

define method cl-mismatch
    (sequence1 :: <list>, sequence2 :: <sequence>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    when (positive?(start1))
      for (i from 0 below start1)
        pop!(sequence1);
      end
    end;
    let last-difference = #f;
    case
      from-end? =>
        unless (end1)
          end1 := start1 + size(sequence1)
        end;
        begin
          let len1 = end1 - start1;
          let len2 = end2 - start2;
          case
            len1 > len2 =>
              let diff = len1 - len2;
	      start1 := start1 + diff;
              last-difference := start1;
              for (i from 0 below diff)
                pop!(sequence1);
              end;
            len2 > len1 =>
              let diff = len2 - len1;
	      start2 := start2 + diff;
              last-difference := start1
          end
        end;
        until (start1 = end1)
          begin
            let elt1 = pop!(sequence1);
            let elt2 = sequence2[start2];
            when (key)
              elt1 := key(elt1);
              elt2 := key(elt2)
            end;
            unless (test(elt1, elt2))
              last-difference := start1 + 1
            end
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end;
        last-difference;
      otherwise =>
        while (#t)
          case
            empty?(sequence1) | (end1 & start1 = end1) =>
              return
                (if (empty?(sequence2) | (end2 & start2 = end2))
                   #f
                 else
                   start1
                 end);
            empty?(sequence2) | (end2 & start2 = end2) =>
              return(start1)
          end;
          let elt1 = pop!(sequence1);
          let elt2 = sequence2[start2];
          when (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(start1)
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end
    end
  end
end method cl-mismatch;

define method cl-mismatch
    (sequence1 :: <sequence>, sequence2 :: <list>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    when (positive?(start2))
      for (i from 0 below start2)
        pop!(sequence2);
      end
    end;
    let last-difference = #f;
    case
      from-end? =>
        unless (end2)
          end2 := start2 + size(sequence2)
        end;
        begin
          let len1 = end1 - start1;
          let len2 = end2 - start2;
          case
            len1 > len2 =>
              let diff = len1 - len2;
	      start1 := start1 + diff;
              last-difference := start1;
            len2 > len1 =>
              let diff = len2 - len1;
	      start2 := start2 + diff;
              last-difference := start1;
              for (i from 0 below diff)
                pop!(sequence2);
              end
          end
        end;
        until (start1 = end1)
          begin
            let elt1 = sequence1[start1];
            let elt2 = pop!(sequence2);
            when (key)
              elt1 := key(elt1);
              elt2 := key(elt2)
            end;
            unless (test(elt1, elt2))
              last-difference := start1 + 1
            end
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end;
        last-difference;
      otherwise =>
        while (#t)
          case
            empty?(sequence1) | (end1 & start1 = end1) =>
              return
                (if (empty?(sequence2) | (end2 & start2 = end2))
                   #f
                 else
                   start1
                 end);
            empty?(sequence2) | (end2 & start2 = end2) =>
              return(start1)
          end;
          let elt1 = sequence1[start1];
          let elt2 = pop!(sequence2);
          when (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(start1)
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end
    end
  end
end method cl-mismatch;

define method cl-mismatch
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?) => index :: false-or(<integer>);
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    let last-difference = #f;
    case
      from-end? =>
        begin
          let len1 = end1 - start1;
          let len2 = end2 - start2;
          case
            len1 > len2 =>
              let diff = len1 - len2;
	      start1 := start1 + diff;
              last-difference := start1;
            len2 > len1 =>
              let diff = len2 - len1;
	      start2 := start2 + diff;
              last-difference := start1
          end
        end;
        for (i1 from end1 - 1 to start1 by -1,
             i2 from end2 - 1 to start2 by -1)
	  let elt1 = sequence1[i1];
	  let elt2 = sequence2[i2];
          when (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(i1 + 1)
          end;
        finally return(last-difference);
        end;
        last-difference;
      otherwise =>
        while (#t)
          case
            empty?(sequence1) | (end1 & start1 = end1) =>
              return
                (if (empty?(sequence2) | (end2 & start2 = end2))
                   #f
                 else
                   start1
                 end);
            empty?(sequence2) | (end2 & start2 = end2) =>
              return(start1)
          end;
          let elt1 = sequence1[start1];
          let elt2 = sequence2[start2];
          when (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(start1)
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end
    end
  end
end method cl-mismatch;


define method cl-merge
    (result-type, sequence1 :: <sequence>, sequence2 :: <sequence>, predicate,
     #key key)
  //--- Just forget about this?
end method cl-merge;

// Following additions by Peter Norvig

define method cl-reduce-compares (op :: <function>)
  method (args :: <list>)
    block (return)
      while (~ empty?(tail(args)))
        if (~ op(first(args),second(args)))
          return(#f)
        else
          args := tail(args)
        end if
      end while
    end block
  end method
end method cl-reduce-compares;
