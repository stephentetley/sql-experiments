/// test-cooper2.flix

namespace TestCooper2 {

use Cooper2/SQLTarget.Query.{Select};
use Cooper2/SQLTarget.SelectFromWhere.{SelectFromWhere};
use Cooper2/SQLTarget.TableAlias.{TableAlias};
use Cooper2/SQLTarget.Selector.{As};
use Cooper2/SQLTarget.Expr.{FieldProjection, Constant};
use Cooper2/SQLTarget.Constant.{Int, String};

@test
def test01(): Int32 & Impure = 
    let q1 = Select(
        SelectFromWhere(
          As(FieldProjection("e1", "first_name"), "name") :: As(FieldProjection("e1", "age_in_years"), "age") :: Nil
        , TableAlias("employee", "e1") :: Nil
        , Constant(Int(1))      /// Makes valid but vacuous where clause
        )
    );
    let ans = Text/PrettyPrint.render(80, Cooper2/Pretty.pretty(q1));
    Console.printLine(ans);
    0

}
