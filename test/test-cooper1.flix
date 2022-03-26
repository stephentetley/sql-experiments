/// test-cooper1.flix


namespace TestCooper {

use Cooper1/SqlFragment.Query.{Select};
use Cooper1/SqlFragment.SelectFromWhere.{SelectFromWhere};
use Cooper1/SqlFragment.TableAlias.{TableAlias};
use Cooper1/SqlFragment.Selector.{As};
use Cooper1/SqlFragment.Expr.{FieldProjection, Constant};
use Cooper1/SqlFragment.Constant.{Int, String};
use Cooper1/SqlFragment.{ppQuery};

@test
def test01(): Unit & Impure = 
    let q1 = Select(
        SelectFromWhere(
          As(FieldProjection("e1", "first_name"), "name") :: As(FieldProjection("e1", "age_in_years"), "age") :: Nil
        , TableAlias("employee", "e1") :: Nil
        , Constant(Int(1))      /// Makes valid but vacuous where clause
        )
    );
    let ans = Text/PrettyPrint.render(80, ppQuery(q1));
    Console.printLine(ans)

}    