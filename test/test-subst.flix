/// test-subst.flix

namespace TestSubst {

use Substitution/IdInt.{Term0, Term, toIdInt, whnf, ppTerm};
use Substitution/Term.Term.{Var, Lam, App};


@test
def test01(): Unit & Impure =
    let term1: Term0 = Lam("f", Lam("t", Var("f")));
    let term2 = whnf(toIdInt(term1));
    let ans = Text/PrettyPrint.render(80, ppTerm(term2));
    Console.printLine(ans)

}
