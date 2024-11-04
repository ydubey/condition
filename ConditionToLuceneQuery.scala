import org.apache.lucene.search._
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur

object ConditionToLuceneQuery {

  def apply(condition: Condition): BooleanQuery = {
    val mainQueryBuilder = new BooleanQuery.Builder
    mainQueryBuilder.add(new TermQuery(new Term("type", "IndexEntry")), Occur.MUST)

    def innerQuery(innerCondition: Condition, innerQueryBuilder: BooleanQuery.Builder = new BooleanQuery.Builder, occur: Occur = Occur.MUST): BooleanQuery = {
      innerCondition match {

        case Condition.And(conditions) =>
          val andQueryBuilder = new BooleanQuery.Builder
          conditions.foreach { c =>
            innerQuery(c, andQueryBuilder)
          }
          innerQueryBuilder.add(new BooleanClause(andQueryBuilder.build(), occur))

        case Condition.Or(conditions) =>
          val orQueryBuilder = new BooleanQuery.Builder
          orQueryBuilder.setMinimumNumberShouldMatch(1)
          conditions.foreach { c =>
            innerQuery(c, orQueryBuilder, Occur.SHOULD)
          }
          innerQueryBuilder.add(new BooleanClause(orQueryBuilder.build(), occur))

        case Condition.Equals(field, value) =>
          value match {
            case ConditionArg.JsonValueArg(s: JsonValue.JString) =>
              innerQueryBuilder.add(new TermQuery(new Term(field, s.value)), occur)
            case o => throw new IllegalArgumentException(s"Unhandled condition type: ${o.productPrefix}")
          }

        case Condition.Like(field, s) =>
          innerQueryBuilder.add(new WildcardQuery(new Term(field, s.value)), occur)

        case Condition.Not(v) => innerQuery(v, innerQueryBuilder, occur = Occur.MUST_NOT)
        case o if o == Condition.Empty => ()
        case o => throw new IllegalArgumentException(s"Unhandled condition type: ${o.productPrefix}")
      }

      innerQueryBuilder.build()
    }

    innerQuery(condition, mainQueryBuilder)
    mainQueryBuilder.build()
  }

}
