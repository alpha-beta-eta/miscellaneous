datatype exp =
    Zero
  | One
  | Char of char
  | Plus of exp * exp
  | Times of exp * exp
  | Star of exp

fun deltap(Zero) = false
  | deltap(One) = true
  | deltap(Char(c)) = false
  | deltap(Plus(e1,e2)) =
  deltap(e1) orelse deltap(e2)
  | deltap(Times(e1,e2)) =
  deltap(e1) andalso deltap(e2)
  | deltap(Star(e)) = true

fun ZeroC() = Zero;
fun OneC() = One;
fun PlusC(Zero,e2) = e2
  | PlusC(e1,Zero) = e1
  | PlusC(e1,e2) = Plus(e1,e2)
fun TimesC(Zero,e2) = Zero
  | TimesC(e1,Zero) = Zero
  | TimesC(One,e2) = e2
  | TimesC(e1,One) = e1
  | TimesC(e1,e2) = Times(e1,e2)
fun StarC(Zero) = One
  | StarC(One) = One
  | StarC(exp) = Star(exp)

fun D(Zero,c) = Zero
  | D(One,c) = Zero
  | D(Char(c0),c) =
  if (c0 = c) then One else Zero
  | D(Plus(e1,e2),c) =
  PlusC(D(e1,c),D(e2,c))
  | D(Times(e1,e2),c) =
  let
   val e3 = TimesC(D(e1,c),e2)
  in
   if deltap(e1)
   then PlusC(e3,D(e2,c))
   else e3
  end
  | D(Star(e),c) =
  TimesC(D(e,c),StarC(e))

fun matchp(str,exp) =
  let
   val l = size(str)
   fun iter(i,exp) =
   if (i = l) then deltap(exp)
   else if (exp = Zero) then false
   else iter(i+1,D(exp,String.sub(str,i)))
  in
   iter(0,exp)
  end

val L_pair_elim =
Times(Char(#"c"),
  Times(Plus(Char(#"a"),Char(#"d")),
    Times(Star(Plus(Char(#"a"),Char(#"d"))),
      Char(#"r"))))