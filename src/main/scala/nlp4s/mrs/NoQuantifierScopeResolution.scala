package nlp4s.mrs

import nlp4s.base.NlpError

case class NoQuantifierScopeResolution() extends NlpError("Could not resolve quantifier scopes")
