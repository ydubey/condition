import org.apache.lucene.search.Query
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.{CharArraySet, StopFilter, StopwordAnalyzerBase, TokenStream}

import java.io.Reader

import scala.util.Try

object LuceneQueryParser {

  def apply(s: String): Either[Throwable, Query] =
    Try(queryParser.parse(s)).toEither

  private lazy val queryParser = new QueryParser("", CaseSensitiveStandardAnalyzer)

  object CaseSensitiveStandardAnalyzer extends StopwordAnalyzerBase(CharArraySet.EMPTY_SET) {

    private val maxTokenLength: Int = 255

    override protected def createComponents(fieldName: String): TokenStreamComponents = {
      val src = new StandardTokenizer()
      src.setMaxTokenLength(maxTokenLength)
      val tok = new StopFilter(src, stopwords)
      new TokenStreamComponents((r: Reader) => {
        src.setMaxTokenLength(maxTokenLength)
        src.setReader(r)
      }, tok)
    }

    override protected def normalize(fieldName: String, in: TokenStream): TokenStream = in
  }

}
