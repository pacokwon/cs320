package cs320

package object proj03 extends Project03 {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)
  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._

    case class TEnv(vars: Map[String, (List[String], Type, Boolean)] = Map(), tbinds: Map[String, (List[String], List[Variant])] = Map(), tvars: Set[String] = Set()) {
      def +(x: String, tparams: List[String], t: Type, mut: Boolean = false): TEnv =
        copy(vars + (x -> (tparams, t, mut)), tbinds)
      def +(x: String, cs: (List[String], List[Variant])): TEnv =
        copy(vars, tbinds + (x -> cs))
      def +(x: String): TEnv =
        copy(vars, tbinds, tvars + x)
    }

    def same(left: Type, right: Type): Boolean =
      (left, right) match {
        case (IntT, IntT) => true
        case (BooleanT, BooleanT) => true
        case (UnitT, UnitT) => true
        case (ArrowT(p1, r1), ArrowT(p2, r2)) =>
          (p1.length == p2.length) && (p1 zip p2).foldLeft(true)((acc, pp) => acc && same(pp._1, pp._2)) && same(r1, r2)
        case (VarT(v1), VarT(v2)) => v1 == v2
        case (AppT(n1, ta1), AppT(n2, ta2)) =>
          (n1 == n2) && (ta1.length == ta2.length) && (ta1 zip ta2).foldLeft(true)((acc, tt) => acc && same(tt._1, tt._2))
        case (_, _) => false
      }

    def mustSame(t1: Type, t2: Type): Type =
      if (same(t1, t2)) t1
      else error("Two types are not equal!")

    def validType(ty: Type, env: TEnv): Type =
      ty match {
        case IntT | BooleanT | UnitT => ty
        case ArrowT(p, r) =>
          ArrowT(p.map(pt => validType(pt, env)), validType(r, env))
        case AppT(name, targs) =>
          targs.foreach(ta => validType(ta, env))
          if (!env.tbinds.contains(name))
            error("Type not in domain!")
          else {
            val tbind = env.tbinds.getOrElse(name, error("Type not in Type Environment!"))
            if (targs.length == tbind._1.size) ty
            else error("# of type arguments != # of type parameters!")
          }
        case VarT(name) =>
          if (env.tvars.contains(name)) ty
          else error("Variable Type not in Type Environment!")
      }

    def validType(rd: RecDef, env: TEnv): Unit =
      rd match {
        case Lazy(name, typ, expr) =>
          mustSame(validType(typ, env), typeCheckHelper(expr, env))
        case RecFun(name, tparams, params, rtype, body) =>
          // (1), (2)
          if (tparams.foldLeft(false)((acc, tparam) => acc || env.tvars.contains(tparam))) {
            error("Variant already in type environment!")
            return
          }
          // (3), (4)
          val tvEnv = tparams.foldLeft(env)((acc, tparam) => acc + tparam)
          // (5)
          params.foreach(param => validType(param._2, tvEnv))
          // (6)
          validType(rtype, tvEnv)
          // (7)
          val nEnv = params.foldLeft(tvEnv)((acc, param) => acc.+(param._1, Nil, param._2))
          // (8), (9)
          mustSame(rtype, typeCheckHelper(body, nEnv))
        case TypeDef(name, tparams, variants) =>
          // (1), (2)
          if (tparams.foldLeft(false)((acc, tparam) => acc || env.tvars.contains(tparam))) {
            error("τ is not well formed!")
            return
          }
          // (3), (4)
          val tvEnv = tparams.foldLeft(env)((acc, tparam) => acc + tparam)
          // (5), (6)
          variants.foreach(variant => variant.params.foreach(param => validType(param, tvEnv)))
      }

    def accTEnv(rd: RecDef, env: TEnv): TEnv =
      rd match {
        case Lazy(name, typ, expr) =>
          env.+(name, Nil, typ)
        case RecFun(name, tparams, params, rtype, body) =>
          env.+(name, tparams, ArrowT(params.map(param => param._2), rtype))
        case TypeDef(name, tparams, variants) =>
          if (env.tbinds.contains(name))
            return error(s"$name already in type environment!")
          variants.foldLeft(env.+(name, (tparams, variants)))((accEnv, variant) =>
            if (variant.params.isEmpty)
              accEnv.+(variant.name, tparams, AppT(name, tparams.map(tp => VarT(tp))))
            else
              accEnv.+(variant.name, tparams, ArrowT(variant.params, AppT(name, tparams.map(tp => VarT(tp)))))
          )
      }

    def substitute(t1: Type, tMap: Map[String, Type]): Type =
      t1 match {
        case IntT | BooleanT | UnitT => t1
        case ArrowT(ptypes, rtype) => ArrowT(ptypes.map(pt => substitute(pt, tMap)), substitute(rtype, tMap))
        case AppT(n, targs) => AppT(n, targs.map(ta => substitute(ta, tMap)))
        case VarT(n) => tMap.foldLeft(t1)((acc, cur) => if (n == cur._1) cur._2 else acc)
      }

    def typeCheck(expr: Expr): Type =
      typeCheckHelper(expr, TEnv())

    def typeCheckHelper(expr: Expr, env: TEnv): Type =
      expr match {
        case IntE(_) => IntT
        case BooleanE(_) => BooleanT
        case UnitE => UnitT
        case Add(left, right) => mustSame(mustSame(IntT, typeCheckHelper(left, env)), typeCheckHelper(right, env))
        case Mul(left, right) => mustSame(mustSame(IntT, typeCheckHelper(left, env)), typeCheckHelper(right, env))
        case Div(left, right) => mustSame(mustSame(IntT, typeCheckHelper(left, env)), typeCheckHelper(right, env))
        case Mod(left, right) => mustSame(mustSame(IntT, typeCheckHelper(left, env)), typeCheckHelper(right, env))
        case Eq(left, right) =>
          mustSame(mustSame(IntT, typeCheckHelper(left, env)), typeCheckHelper(right, env))
          BooleanT
        case Lt(left, right) =>
          mustSame(mustSame(IntT, typeCheckHelper(left, env)), typeCheckHelper(right, env))
          BooleanT
        case Sequence(left, right) =>
          typeCheckHelper(left, env)
          typeCheckHelper(right, env)
        case If(cond, texpr, fexpr) =>
          mustSame(BooleanT, typeCheckHelper(cond, env))
          mustSame(typeCheckHelper(texpr, env), typeCheckHelper(fexpr, env))
        case Val(mut, name, typ, exp, body) =>
          typ match {
            case Some(t) =>
              mustSame(validType(t, env), typeCheckHelper(exp, env))
            case None =>
          }
          val t1 = typeCheckHelper(exp, env)
          typeCheckHelper(body, env.+(name, Nil, t1, mut))
        case Id(name, targs) =>
          // (1), (2)
          targs.foreach(targ => validType(targ, env))
          // (3), (4)
          val (tparams, typ, mut) = env.vars.getOrElse(name, error(s"$name is not in type environment!"))
          // (5)
          if (targs.length != tparams.length)
            return error("# of type arguments != # of type parameters!")
          // (6), (7), (8)
          substitute(typ, (tparams zip targs).toMap)
        case RecBinds(defs, body) =>
          // (1), (2)
          val tenv = defs.foldLeft(env)((acc, rd) => accTEnv(rd, acc))
          defs.foreach(rd => validType(rd, tenv))
          validType(typeCheckHelper(body, tenv), env)
        case Fun(params, body) =>
          // (1), (2)
          params.foreach(param => validType(param._2, env))
          // (3)
          val newEnv = params.foldLeft(env)((acc, param) => acc.+(param._1, Nil, param._2))
          // (4), (5)
          ArrowT(params.map(param => param._2), typeCheckHelper(body, newEnv))
        case Assign(name, exp) =>
          // (1), (2), (3)
          val (tparams, typ, mut) = env.vars.getOrElse(name, error(s"$name is not in type environment!"))
          // (4)
          if (tparams.length != 0)
            return error("type parameter's length is over 0!")
          // (5)
          if (!mut)
            return error("variable is not mutable!")
          // (6)
          mustSame(typ, typeCheckHelper(exp, env))
          // (7)
          UnitT
        case App(fun, args) =>
          // (1)
          typeCheckHelper(fun, env) match {
            // (2), (3)
            case at @ ArrowT(ptypes, rtype) =>
              // (4)
              if (ptypes.length != args.length)
                error("# of type arguments != # of type parameters!")
              else {
                // (5)
                (ptypes zip args).foreach(tatup => mustSame(tatup._1, typeCheckHelper(tatup._2, env)))
                // (6)
                rtype
              }
            case default => error("type is not function!")
          }
        case Match(exp, cases) =>
          typeCheckHelper(exp, env) match {
            // (3), (4)
            case AppT(name, targs) =>
              // (5), (6)
              val (alphas, variants) = env.tbinds.getOrElse(name, error("type is not AppT!"))
              // (7)
              if (targs.length != alphas.length)
                return error("# of type arguments != # of type parameters!")
              // (8)
              if (cases.length != variants.length)
                return error("# of cases != # of variants!")

              val caseTypes = cases.map(esac => {
                val filtered = variants.filter(variant => variant.name == esac.variant)
                if (filtered.isEmpty)
                  error(s"No variant named ${esac.variant}")
                val Variant(name, params) = filtered.head
                if (esac.names.length != params.length)
                  error("# of variant parameters != # of case arguments")

                // for each variant type param
                val newEnv = (esac.names zip params).foldLeft(env)((accEnv, nptup) => {
                  // for each alpha
                  val subbed = substitute(nptup._2, (alphas zip targs).toMap)
                  accEnv.+(nptup._1, Nil, subbed)
                })

                typeCheckHelper(esac.body, newEnv)
              })

              caseTypes.foldLeft(caseTypes.head)((acc, ct) => mustSame(acc, ct))
            case _ => error("Not AppT!")
          }
      }
  }

  object U {
    import Untyped._

    type Sto = Map[Addr, Value]

    def malloc(sto: Sto): Addr =
      sto.foldLeft(0) {
        case (max, (addr, _)) => math.max(max, addr)
      } + 1

    def envLookup(id: String, env: Env): Addr =
      env.getOrElse(id, error(s"Free identifier $id!"))

    def storeLookup(addr: Addr, sto: Sto): Value =
      sto.getOrElse(addr, error("Address does not exist!"))

    def accEnv(rd: RecDef, env: Env, sto: Sto): (Env, Sto) =
      rd match {
        case Lazy(name, expr) =>
          val addr = malloc(sto)
          (Map(name -> addr), sto + (addr -> UnitV))
        case RecFun(name, params, body) =>
          val addr = malloc(sto)
          (Map(name -> addr), sto + (addr -> UnitV))
        case TypeDef(variants) =>
          variants.foldLeft((Map[String, Addr](), sto))((acc, variant) => {
            val addr = malloc(acc._2)
            (acc._1 + (variant.name -> addr), acc._2 + (addr -> UnitV))
          })
      }

    def accSto(rd: RecDef, env: Env, sto: Sto): Sto =
      rd match {
        case Lazy(name, expr) =>
          sto + (envLookup(name, env) -> ExprV(expr, env))
        case RecFun(name, params, body) =>
          sto + (envLookup(name, env) -> CloV(params, body, env))
        case TypeDef(variants) =>
          variants.foldLeft(sto)((acc, variant) =>
            acc + (
              envLookup(variant.name, env) -> (
                if (variant.empty)
                  VariantV(variant.name, Nil)
                else
                  ConstructorV(variant.name)
              )
            )
          )
      }

    def interp(expr: Expr): Value =
      interpE(expr, Map(), Map())._1

    def interpE(expr: Expr, env: Env, sto: Sto): (Value, Sto) =
      expr match {
        case IntE(value) => (IntV(value), sto)
        case BooleanE(value) => (BooleanV(value), sto)
        case UnitE => (UnitV, sto)
        case Add(left, right) =>
          val (IntV(n), ls) = interpE(left, env, sto)
          val (IntV(m), rs) = interpE(right, env, ls)
          (IntV(n + m), rs)
        case Mul(left, right) =>
          val (IntV(n), ls) = interpE(left, env, sto)
          val (IntV(m), rs) = interpE(right, env, ls)
          (IntV(n * m), rs)
        case Div(left, right) =>
          val (IntV(n), ls) = interpE(left, env, sto)
          val (IntV(m), rs) = interpE(right, env, ls)
          (IntV(n / m), rs)
        case Mod(left, right) =>
          val (IntV(n), ls) = interpE(left, env, sto)
          val (IntV(m), rs) = interpE(right, env, ls)
          (IntV(n % m), rs)
        case Eq(left, right) =>
          val (IntV(n), ls) = interpE(left, env, sto)
          val (IntV(m), rs) = interpE(right, env, ls)
          (BooleanV(n == m), rs)
        case Lt(left, right) =>
          val (IntV(n), ls) = interpE(left, env, sto)
          val (IntV(m), rs) = interpE(right, env, ls)
          (BooleanV(n < m), rs)
        case Sequence(left, right) =>
          val (_, ls) = interpE(left, env, sto)
          interpE(right, env, ls)
        case If(c, t, f) =>
          val (v, cs) = interpE(c, env, sto)
          v match {
            case BooleanV(b) =>
              interpE(if (b) t else f, env, cs)
            case _ => error("Not BooleanV!")
          }
        case Val(name, expr, body) =>
          val (v, ls) = interpE(expr, env, sto)
          val addr = malloc(ls)
          interpE(body, env + (name -> addr), ls + (addr -> v))
        case Id(name) =>
          // (1), (3), (4)
          val addr = envLookup(name, env)
          // (2), (5), (6)
          storeLookup(addr, sto) match {
            case ExprV(exp, lEnv) =>
              val (lV, lM) = interpE(exp, lEnv, sto)
              (lV, lM + (addr -> lV))
            case default => (default, sto)
          }
        case RecBinds(defs, body) =>
          val (aEnv, aSto) = defs.foldLeft((Map[String, Addr](), sto))((acc, rd) => {
            val (nenv, nsto) = accEnv(rd, acc._1, acc._2)
            (acc._1 ++ nenv, nsto)
          })
          val nEnv = env ++ aEnv
          val nSto = defs.foldLeft(aSto)((acc, rd) => accSto(rd, nEnv, acc))
          interpE(body, nEnv, nSto)
        case Fun(params, body) =>
          (CloV(params, body, env), sto)
        case Assign(name, exp) =>
          if (!env.contains(name))
            return error(s"$name is not in environment!")
          val (nv, ns) = interpE(exp, env, sto)
          (UnitV, ns + (envLookup(name, env) -> nv))
        case App(fun, args) =>
          interpE(fun, env, sto) match {
            case (CloV(params, body, fenv), ns) =>
              // (4), (5) - (c)
              val (valsRev, valsSto) = args.foldLeft((List[Value](), ns))((acc, arg) => {
                val (nv, ns) = interpE(arg, env, acc._2)
                (nv :: acc._1, ns)
              })
              val vals = valsRev.reverse

              // (5) - (a), (b)
              if (args.length != params.length)
                return error("# of type arguments != # of type parameters!")

              // (5) - (d), (e), (f)
              val (newFEnv, newSto) = (params zip vals).foldLeft((fenv, valsSto))((acc, pzv) => {
                val addr = malloc(acc._2)
                (acc._1 + (pzv._1 -> addr), acc._2 + (addr -> pzv._2))
              })

              // (5) - (g), (h), (i)
              interpE(body, newFEnv, newSto)
            case (ConstructorV(name), ns) =>
              // (4)
              val (valsRev, nsto) = args.foldLeft((List[Value](), sto))((acc, arg) => {
                val (nv, ns) = interpE(arg, env, acc._2)
                (nv :: acc._1, ns)
              })
              val vals = valsRev.reverse
              // (6)
              (VariantV(name, vals), nsto)
            case (_, _) =>
              error("Not Closure or Constructor!")
          }
        case Match(exp, cases) =>
          interpE(exp, env, sto) match {
            case (VariantV(name, values), ns) =>
              val fcases = cases.filter(esac => esac.variant == name)
              if (fcases.isEmpty)
                error("Variant does not match any case!")
              val Case(variant, names, body) = fcases.head
              if (values.length != names.length)
                error("# of variant parameters != # of case arguments")
              val (newEnv, newSto) = (names zip values).foldLeft((env, ns))((acc, nv) => {
                val addr = malloc(acc._2)
                (acc._1 + (nv._1 -> addr), acc._2 + (addr -> nv._2))
              })
              interpE(body, newEnv, newSto)
            case (_, _) => error("Not Variant!")
          }
      }
  }

  def tests: Unit = {
    // test-int
    test(run("42"), "42")
    // test-boolean
    test(run("true"), "true")
    // test-unit
    test(run("()"), "()")

    // test-add
    test(run("1 + 2"), "3")
    // test-mul
    test(run("2 * 4"), "8")
    // test-div
    test(run("5 / 2"), "2")
    // test-mod
    test(run("13 % 5"), "3")
    // test-eq
    test(run("1 == 1"), "true")
    // test-lt
    test(run("1 < 1"), "false")
    // test-seq
    test(run("{1; 2}"), "2")

    // test-if
    test(run("if (true) 1 else 2"), "1")

    // test-val
    test(run("""
      val x = 1 + 2;
      val y: Int = x * 4 + 1;
      y / (x - 1)
    """), "6")
    // test-lazy
    test(run("""
      lazy val f: Int => Int = (x: Int) => if (x < 1) 0 else x + f(x - 1);
      f(10)
    """), "55")
    // test-rec
    test(run("""
      def f(x: Int): Int = if (x < 1) 0 else x + f(x - 1);
      f(10)
    """), "55")

    // test-fun
    test(run("(x: Int) => x + x"), "<function>")
    // test-app
    test(run("((x: Int, y: Int) => x + y)(1, 2)"), "3")

    // test-var-assign
    test(run("""
      var x = 1;
      var y: Int = x * 4 + 8;
      { x = 3; y / (x - 1) }
    """), "6")

    // test-type-match
    test(run("""
      type Fruit {
        case Apple
        case Banana(Int)
      }
      (Apple match {
        case Apple => 1
        case Banana(x) => 0
      }) + (Banana(1) match {
        case Apple => 0
        case Banana(x) => x
      })
    """), "2")

    // test-poly1
    test(run("""
      def f['T, 'S](t: 'T, s: 'S): 'T = t;
      f[Int, Boolean](1, true)
    """), "1")
    // test-poly2
    test(run("""
      type Fruit['T] {
        case Apple
        case Banana('T)
      }
      (Apple[Boolean] match {
        case Apple => 1
        case Banana(x) => 0
      }) + (Banana[Int](1) match {
        case Apple => 0
        case Banana(x) => x
      })
    """), "2")

    // test-primitive
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

{
check(!intEquals(1, 2));
check(intEquals(3, 3));
check(intMax(3, 6) == 6);
check(intMin(3, 6) == 3);
check(!booleanEquals(true, false));
check(booleanEquals(true, true));
check(unitEquals((), ()));

score
}"""
      ),
      "7"
    )

    // test-pair
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val p1 = Pair[Int, Boolean](1, true);
val p2 = Pair[Int, Boolean](1, false);
val p3 = Pair[Int, Boolean](2, true);

val eq = pairEquals[Int, Boolean](intEquals, booleanEquals);

{
check(pairFst[Int, Boolean](p1) == 1);
check(pairSnd[Int, Boolean](p1));
check(pairFst[Int, Boolean](p2) == 1);
check(!pairSnd[Int, Boolean](p2));
check(pairFst[Int, Boolean](p3) == 2);
check(pairSnd[Int, Boolean](p3));
check(eq(p1, p1));
check(!eq(p1, p2));
check(!eq(p1, p3));

score
}"""
      ),
      "9"
    )

    // test-option
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val opt1 = Some[Int](1);
val opt2 = optionMap[Int, Int](opt1, (x: Int) => x + x);
val opt3 = optionFilter[Int](opt1, (x: Int) => x < 2);
val opt4 = optionFilter[Int](opt2, (x: Int) => x < 2);
val opt5 = optionFlatten[Int](Some[Option[Int]](opt1));
val opt6 = optionFlatten[Int](Some[Option[Int]](opt4));
val opt7 = optionFlatten[Int](None[Option[Int]]);

def aux(i: Int): Option[Int] =
  if (i == 1) Some[Int](i) else None[Int];

val opt8 = optionFlatMap[Int, Int](opt1, aux);
val opt9 = optionFlatMap[Int, Int](opt2, aux);
val opt10 = optionFlatMap[Int, Int](opt4, aux);
val opt11 = optionFilterNot[Int](opt1, (x: Int) => x < 2);
val opt12 = optionFilterNot[Int](opt2, (x: Int) => x < 2);

val eq = optionEquals[Int](intEquals);
val eql = listEquals[Int](intEquals);

{
check(eq(Some[Int](1), Some[Int](1)));
check(!eq(Some[Int](1), Some[Int](2)));
check(!eq(Some[Int](1), None[Int]));
check(eq(None[Int], None[Int]));
check(eq(opt1, Some[Int](1)));
check(eq(opt2, Some[Int](2)));
check(eq(opt3, Some[Int](1)));
check(eq(opt4, None[Int]));
check(eq(opt5, Some[Int](1)));
check(eq(opt6, None[Int]));
check(eq(opt7, None[Int]));
check(eq(opt8, Some[Int](1)));
check(eq(opt9, None[Int]));
check(eq(opt10, None[Int]));
check(eq(opt11, None[Int]));
check(eq(opt12, Some[Int](2)));
check(!optionIsEmpty[Int](opt1));
check(optionIsEmpty[Int](opt4));
check(optionNonEmpty[Int](opt1));
check(!optionNonEmpty[Int](opt4));
check(eql(optionToList[Int](opt1), List1[Int](1)));
check(eql(optionToList[Int](opt4), List0[Int]()));
check(optionGetOrElse[Int](opt1, 0) == 1);
check(optionGetOrElse[Int](opt4, 0) == 0);
optionForeach[Int](opt1, (i: Int) => check(i == 1));
optionForeach[Int](opt4, (i: Int) => check(true));

score
}"""
      ),
      "25"
    )

    // test-box
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val b = Box[Int](1);
val i1 = boxGet[Int](b);
val i2 = boxSet[Int](b, 2);
val i3 = boxGet[Int](b);
val i4 = boxSet[Int](b, 1);
val i5 = boxGet[Int](b);

{
check(i1 == 1);
check(i2 == 1);
check(i3 == 2);
check(i4 == 2);
check(i5 == 1);

score
}"""
      ),
      "5"
    )

    // test-list
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val l0 = List5[Int](1, 2, 3, 4, 5);
val l1 = List3[Int](1, 2, 3);
val l2 = List2[Int](4, 5);
val zipped0 = listZip[Int, Int](l0, l0);
val unzipped0 = listUnzip[Int, Int](zipped0);
val l3 = pairFst[List[Int], List[Int]](unzipped0);
val l4 = pairSnd[List[Int], List[Int]](unzipped0);
val zipped1 = listZip[Int, Int](l0, l1);
val unzipped1 = listUnzip[Int, Int](zipped1);
val l5 = pairFst[List[Int], List[Int]](unzipped1);
val l6 = pairSnd[List[Int], List[Int]](unzipped1);
val zipped2 = listZipWithIndex[Int](l0);
val unzipped2 = listUnzip[Int, Int](zipped2);
val l7 = pairFst[List[Int], List[Int]](unzipped2);
val l8 = pairSnd[List[Int], List[Int]](unzipped2);

val eq = listEquals[Int](intEquals);
val eqo = optionEquals[Int](intEquals);
def odd(n: Int): Boolean = n % 2 != 0;
def lt4(n: Int): Boolean = n < 4;

{
check(eq(l0, l0));
check(!eq(l0, l1));
check(!eq(l0, l2));
check(!eq(l1, l2));
check(!eq(l0, Nil[Int]));
check(eq(Nil[Int], Nil[Int]));
check(eq(listAppended[Int](listAppended[Int](l1, 4), 5), l0));
check(eq(listConcat[Int](l1, l2), l0));
check(listCount[Int](l0, odd) == 3);
check(eq(listDrop[Int](l0, 3), l2));
check(listExists[Int](l0, lt4));
check(!listExists[Int](l2, lt4));
check(eq(listFilter[Int](l0, lt4), l1));
check(eq(listFilterNot[Int](l0, lt4), l2));
check(eqo(listFind[Int](l0, lt4), Some[Int](1)));
check(eqo(listFind[Int](l2, lt4), None[Int]));
check(eq(listFlatMap[Int, Int](l1, (n: Int) => if (n == 1) l1 else if (n == 2) l2 else Nil[Int]), l0));
check(eq(listFlatten[Int](List2[List[Int]](l1, l2)), l0));
check(listFoldLeft[Int, Int](0, l0, (n: Int, m: Int) => n + m) == 15);
check(listFoldRight[Int, Int](l0, 0, (n: Int, m: Int) => n + m) == 15);
check(!listForall[Int](l0, lt4));
check(listForall[Int](l1, lt4));
listForeach[Int](l0, (n: Int) => check(odd(n)));
check(eqo(listGet[Int](l0, 4), Some[Int](5)));
check(eqo(listGet[Int](l0, 5), None[Int]));
check(!listIsEmpty[Int](l0));
check(listIsEmpty[Int](Nil[Int]));
check(listLength[Int](l0) == 5);
check(eq(listMap[Int, Int](l0, (n: Int) => n * n), List5[Int](1, 4, 9, 16, 25)));
check(listNonEmpty[Int](l0));
check(!listNonEmpty[Int](Nil[Int]));
check(eq(listPrepended[Int](listPrepended[Int](listPrepended[Int](l2, 3), 2), 1), l0));
check(eq(listReverse[Int](l0), List5[Int](5, 4, 3, 2, 1)));
check(eq(listTake[Int](l0, 3), l1));
check(eq(l0, l3));
check(eq(l0, l4));
check(eq(l1, l5));
check(eq(l1, l6));
check(eq(l0, l7));
check(eq(l0, listMap[Int, Int](l8, (n: Int) => n + 1)));

score
}"""
      ),
      "42"
    )

    // test-map
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val m0 = Map1[Int, Int](intEquals, 0, 0);
val m1 = mapUpdated[Int, Int](m0, 1, 2);
val m2 = mapUpdated[Int, Int](m1, 2, 4);
val m3 = mapUpdated[Int, Int](m2, 3, 6);
val m4 = mapRemoved[Int, Int](m3, 2);
val m5 = mapUpdated[Int, Int](m2, 3, 8);

val eqo = optionEquals[Int](intEquals);

{
check(eqo(mapGet[Int, Int](m0, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m0, 1), None[Int]));
check(eqo(mapGet[Int, Int](m0, 2), None[Int]));
check(eqo(mapGet[Int, Int](m0, 3), None[Int]));
check(eqo(mapGet[Int, Int](m0, 4), None[Int]));

check(eqo(mapGet[Int, Int](m1, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m1, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m1, 2), None[Int]));
check(eqo(mapGet[Int, Int](m1, 3), None[Int]));
check(eqo(mapGet[Int, Int](m1, 4), None[Int]));

check(eqo(mapGet[Int, Int](m2, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m2, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m2, 2), Some[Int](4)));
check(eqo(mapGet[Int, Int](m2, 3), None[Int]));
check(eqo(mapGet[Int, Int](m2, 4), None[Int]));

check(eqo(mapGet[Int, Int](m3, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m3, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m3, 2), Some[Int](4)));
check(eqo(mapGet[Int, Int](m3, 3), Some[Int](6)));
check(eqo(mapGet[Int, Int](m3, 4), None[Int]));

check(eqo(mapGet[Int, Int](m4, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m4, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m4, 2), None[Int]));
check(eqo(mapGet[Int, Int](m4, 3), Some[Int](6)));
check(eqo(mapGet[Int, Int](m4, 4), None[Int]));

check(eqo(mapGet[Int, Int](m4, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m4, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m4, 2), None[Int]));
check(eqo(mapGet[Int, Int](m4, 3), Some[Int](6)));
check(eqo(mapGet[Int, Int](m4, 4), None[Int]));

score
}"""
      ),
      "30"
    )

    // test-string
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

{
check(stringEquals("abc \n"<STRP, EOS>, List5[Int](97, 98, 99, 32, 10)));
check(stringEquals(substring("12abc \n"<STRP, EOS>, 2, 5), List3[Int](97, 98, 99)));
check("abc \n"<(n: Int, m: Int) => n + m, 0> == 336);

score
}"""
      ),
      "3"
    )

    // test-fae
    test(
      runWithStdLib(
"""
type Expr {
  case Num(Int)
  case Add(Expr, Expr)
  case Sub(Expr, Expr)
  case Id(Int)
  case Fun(Int, Expr)
  case App(Expr, Expr)
}

type Value {
  case NumV(Int)
  case CloV(Int, Expr, Map[Int, Value])
}

def interp(e: Expr, env: Map[Int, Value]): Option[Value] = e match {
  case Num(n) => Some[Value](NumV(n))
  case Add(l, r) => optionFlatMap[Value, Value](interp(l, env), (lv: Value) => lv match {
    case NumV(n) => optionFlatMap[Value, Value](interp(r, env),
      (rv: Value) => rv match {
        case NumV(m) => Some[Value](NumV(n + m))
        case CloV(x, e, fenv) => None[Value]
      }
    )
    case CloV(x, e, fenv) => None[Value]
  })
  case Sub(l, r) => optionFlatMap[Value, Value](interp(l, env), (lv: Value) => lv match {
    case NumV(n) => optionFlatMap[Value, Value](interp(r, env),
      (rv: Value) => rv match {
        case NumV(m) => Some[Value](NumV(n - m))
        case CloV(x, e, fenv) => None[Value]
      }
    )
    case CloV(x, e, fenv) => None[Value]
  })
  case Id(x) => mapGet[Int, Value](env, x)
  case Fun(x, e) => Some[Value](CloV(x, e, env))
  case App(f, a) => optionFlatMap[Value, Value](interp(f, env), (fv: Value) => fv match {
    case NumV(n) => None[Value]
    case CloV(x, e, fenv) => optionFlatMap[Value, Value](interp(a, env),
      (av: Value) => interp(e, mapUpdated[Int, Value](fenv, x, av))
    )
  })
};

lazy val digit: Parser[Expr] =
  parserMap[Int, Expr](
    () => parserCond((x: Int) => 48 <= x && x < 58),
    (x: Int) => Num(x - 48)
  );

lazy val add: Parser[Expr] =
  parserMap[Pair[Int, Pair[Expr, Expr]], Expr](
    () => parserThen[Int, Pair[Expr, Expr]](
      () => parserConst(43),
      () => parserThen[Expr, Expr](() => e, () => e)
    ),
    (p: Pair[Int, Pair[Expr, Expr]]) =>
      pairSnd[Int, Pair[Expr, Expr]](p) match {
        case Pair(l, r) => Add(l, r)
      }
  );

lazy val sub: Parser[Expr] =
  parserMap[Pair[Int, Pair[Expr, Expr]], Expr](
    () => parserThen[Int, Pair[Expr, Expr]](
      () => parserConst(45),
      () => parserThen[Expr, Expr](() => e, () => e)
    ),
    (p: Pair[Int, Pair[Expr, Expr]]) =>
      pairSnd[Int, Pair[Expr, Expr]](p) match {
        case Pair(l, r) => Sub(l, r)
      }
  );

lazy val id: Parser[Expr] =
  parserMap[Int, Expr](
    () => parserCond((x: Int) => 97 <= x && x <= 122),
    (x: Int) => Id(x)
  );

lazy val fun: Parser[Expr] =
  parserMap[Pair[Int, Pair[Int, Expr]], Expr](
    () => parserThen[Int, Pair[Int, Expr]](
      () => parserConst(47),
      () => parserThen[Int, Expr](
        () => parserCond((x: Int) => 97 <= x && x <= 122),
        () => e
      )
    ),
    (p: Pair[Int, Pair[Int, Expr]]) =>
      pairSnd[Int, Pair[Int, Expr]](p) match {
        case Pair(p, b) => Fun(p, b)
      }
  );

lazy val app: Parser[Expr] =
  parserMap[Pair[Int, Pair[Expr, Expr]], Expr](
    () => parserThen[Int, Pair[Expr, Expr]](
      () => parserConst(64),
      () => parserThen[Expr, Expr](() => e, () => e)
    ),
    (p: Pair[Int, Pair[Expr, Expr]]) =>
      pairSnd[Int, Pair[Expr, Expr]](p) match {
        case Pair(l, r) => App(l, r)
      }
  );

lazy val e: Parser[Expr] =
  parserOr[Expr](
    () => parserOr[Expr](
      () => parserOr[Expr](
        () => parserOr[Expr](
          () => parserOr[Expr](
            () => digit,
            () => add
          ),
          () => sub
        ),
        () => id
      ),
      () => fun
    ),
    () => app
  );

parseAll[Expr](e, "@@/x/y+xy23"<STRP, EOS>) match {
  case None => -1
  case Some(e) => interp(e, Map0[Int, Value](intEquals)) match {
    case None => -2
    case Some(v) => v match {
      case NumV(n) => if (n < 0) -3 else n
      case CloV(x, e, env) => -4
    }
  }
}
"""
      ),
      "5"
    )
  }

}
