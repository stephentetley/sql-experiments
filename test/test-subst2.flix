/// test-subst.flix

namespace TestSubst2 {

use Monad.{>>=};
use Subst2/Syntax.Term;
use Subst2/Syntax.Term.{Var, Cst, Lam, App};
use Subst2/Syntax.Constant.{CNone, CInt, CString};
use Subst2/Syntax.{ppTerm};
use Subst2/Alpha.{ac};
use Subst2/Beta.{cbn};
use Subst2/Mon.{runMon};


@test
def test01(): Unit & Impure =
    println("TestSubst2 test01...");
    let term1 = Lam("x", Lam("x", Var("x")));
    Text/PrettyPrint.render(80, ppTerm(0, term1)) |> println;
    let term2 = runMon(Map.empty(), ac(term1));
    Text/PrettyPrint.render(80, ppTerm(0, term2)) |> println;
    ()


@test
def test02(): Unit & Impure =
    println("TestSubst2 test02 (cbn)...");
    let term1 = App(Lam("x", Var("x")), Cst(CInt(1)));
    Text/PrettyPrint.render(80, ppTerm(0, term1)) |> println;
    let term2 = runMon(Map.empty(), ac(term1) >>= cbn);
    Text/PrettyPrint.render(80, ppTerm(0, term2)) |> println;
    ()


}
