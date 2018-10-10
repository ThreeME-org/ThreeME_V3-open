library(pegr)
library(tools)
library(magrittr)
library(stringr)

# Quick start
# 
# First evaluate the whole file (Run)
# Then, example use:
# teXdoc(c("SU.mdl", "producer.mdl", "prices.mdl", "consumer.mdl", "adjustments.mdl"))
# teXdoc(c("SU.mdl", "producer.mdl", "prices.mdl", "consumer.mdl", "government.mdl",  "Trade_inter.mdl", "demography.mdl", "ghg_emissions.mdl", "adjustments.mdl"),"ThreeMEv3_eqs_01")
rm(list = ls())

peg <- new.parser()


buildCall <- function(name, params) {
  params <- if (is.list(params)) {
    str_c(params, collapse = ", ")
  } else {
    params
  }
  str_c(name, "(", params, ")", collapse = "")
}

str_merge <- function(v, char = '') str_c(v, collapse = char)

buildString <- function(v) str_c('"', str_merge(v), '"')

dynIndices <- function(x) attr(x, "indices")
dynTimeIndex <- function(x) attr(x, "timeIndex")
dynWhere <- function(x) attr(x, "where")
dynCondition <- function(x) attr(x, "condition")
dynParentheses <- function(x) attr(x, "parentheses")

dynIdentity <- function(...) list(...)

custom <- list(
  dynName = dynIdentity,
  dynNum = dynIdentity,
  dynVar = dynIdentity,
  dynExp = dynIdentity,
  dynFun = dynIdentity,
  dynOp  = dynIdentity,
  dynEq  = dynIdentity
)

dynName <- function(s) {
  custom$dynName(s)
}

dynNum <- function(val) {
  x <- as.numeric(val)
  attr(x, "indices") <- NULL
  class(x) <- c(class(x), "dynNum")
  custom$dynNum(x)
}

dynVar <- function(name) dynVarGen(name)
dynVarTime <- function(name, timeIndex) dynVarGen(name, timeIndex = timeIndex)
dynVarIndex <- function(name, indices) dynVarGen(name, indices)
dynVarIndexTime <- function(name, indices, timeIndex) dynVarGen(name, indices, timeIndex)

dynVarGen <- function(name, indices = NULL, timeIndex = NULL) {
  attr(name, "indices") <- indices
  attr(name, "timeIndex") <- timeIndex
  class(name) <- c(class(name), "dynVar")
  custom$dynVar(name, indices, timeIndex)
}

dynExpParen   <- function(lst) dynExpGen(lst, parentheses = TRUE)
dynExpUnary   <- function(op, lst) dynOp(op, dynZero, lst)
dynExpBinary  <- function(lhs, op, rhs) dynOp(op, lhs, rhs)
dynExpIf      <- function(lst, condition) dynExpGen(lst, condition = condition)
dynExpWhere   <- function(lst, where) dynExpGen(lst, where = where)
dynExpIfWhere <- function(lst, condition, where) dynExpGen(lst, condition = condition, where = where)

dynExpGen <- function(lst, indices = NULL, where = NULL, condition = NULL, parentheses = FALSE) {
  attr(lst, "indices")    <- c(dynIndices(lst), indices)
  attr(lst, "where")      <- if (!is.null(where)) {
    attr(lst, "indices")  <- setdiff(attr(lst, "indices"), where)
    where
  } else {
    NULL
  }
  attr(lst, "condition")  <- if (is.null(condition)) {
    dynCondition(condition)
  } else {
    condition
  }
  attr(lst, "parentheses") <- parentheses
  class(lst) <- c(class(lst), "dynExp")
  custom$dynExp(lst, indices, where, condition, parentheses)
}

dynFun <- function(name, .args) {
  attr(name, "args") <- .args
  attr(name, "indices") <- Reduce(c, lapply(.args, dynIndices), c())
  class(name) <- c(class(name), "dynFun")
  custom$dynFun(name, .args)
}

dynOp <- function(op, e1, e2) {
  dynExpGen(custom$dynOp(op, e1, e2),
            c(dynIndices(e1), dynIndices(e2)) %>% unique)
}

dynEq <- function(lhs, rhs, isOver = FALSE) {
  eq <- list(lhs = lhs,
             rhs = rhs)
  class(eq) <- c(class(eq), "dynEq")
  custom$dynEq(lhs, rhs)
}

dynZero <- dynNum(0)

peg <- peg %>% 
  
  add_rule("Space <- ('\t' / ' ')+", act = "list()") %>%
  add_rule("Comma <- Space* ',' Space*", act = "list()") %>%
  add_rule("Equal <- Space* '=' Space*", act = "list()") %>%
  add_rule("Different <- '<>'", act = "'!='") %>%
  add_rule("Division <- '/'", act = "buildString(v)") %>%
  
  add_rule("LBracket <- '[' Space*", act = "list()") %>%
  add_rule("RBracket <- Space* ']'", act = "list()") %>%
  add_rule("LParen <- '(' Space*", act = "list()") %>%
  add_rule("RParen <- Space* ')'", act = "list()") %>%
  add_rule("LCurly <- '{' Space*", act = "list()") %>%
  add_rule("RCurly <- Space* '}'", act = "list()") %>%
  add_rule("Operator <- Space* ('<=' / '>=' / Different / '<' / '>' / '+' / '-' / '/' / '*') Space*", act = "buildString(v)") %>%
  
  add_rule("Name <- ([a-z] / [A-Z] / '_' / '@' / '%') ([0-9] / [a-z] / [A-Z] / '_')*", act = "buildCall('dynName', buildString(v))") %>%
  add_rule("Index <- LBracket (Name Comma)* Name RBracket", act = "buildCall('c', v)") %>%
  add_rule("TimeIndex <- LCurly '-1' RCurly", act = "\"\\\"t-1\\\"\"") %>%
  add_rule("VariableSimple <- Name",                    act = "buildCall('dynVar', v)") %>%
  add_rule("VariableIndex <- Name Index",               act = "buildCall('dynVarIndex', v)") %>%
  add_rule("VariableTime <- Name TimeIndex",            act = "buildCall('dynVarTime', v)") %>%
  add_rule("VariableIndexTime <- Name Index TimeIndex", act = "buildCall('dynVarIndexTime', v)") %>%
  add_rule("Variable <- VariableIndexTime / VariableIndex / VariableTime / VariableSimple") %>%
  
  add_rule("Number <- [0-9]+ / ([0-9]+)? '.' [0-9]+", act = "buildCall('dynNum', buildString(v))") %>%
  add_rule("Function <- Name LParen QualifiedExpression? (Comma Expression)* RParen", act = "buildCall('dynFun', v)") %>%
  add_rule("Term <- Function / Variable / Number") %>%
  add_rule("Fraction <- LParen Expression RParen Space* Division Space* LParen Expression RParen",  act = "buildCall('dynExpBinary', v)") %>%
  add_rule("ParenExp <- LParen Expression RParen",  act = "buildCall('dynExpParen', v)") %>%
  add_rule("UnaryExp <- Operator Space* Expression",     act = "buildCall('dynExpUnary', v)") %>%
  add_rule("BinaryExp <- Term Operator Expression", act = "buildCall('dynExpBinary', v)") %>%
  add_rule("BinaryExpParen <- ParenExp Operator Expression", act = "buildCall('dynExpBinary', v)") %>%
  add_rule("Expression <- Fraction / BinaryExpParen / ParenExp / BinaryExp / UnaryExp / Term") %>%
  
  add_rule("IfKeyword <-  Space+ 'if' Space+", act = "list()") %>%
  add_rule("WhereKeyword <-  Space+ ('where' / 'on') Space+", act = "list()") %>%
  add_rule("If <- IfKeyword Expression") %>%
  add_rule("Where <- WhereKeyword Name (Space+ 'in' Space+ Name)?") %>%
  add_rule("ExpressionIf <- Expression If",            act = "buildCall('dynExpIf', v)") %>%
  add_rule("ExpressionWhere <- Expression Where",      act = "buildCall('dynExpWhere', v)") %>%
  add_rule("ExpressionIfWhere <- Expression If Where", act = "buildCall('dynExpIfWhere', v)") %>%
  add_rule("QualifiedExpression <- ExpressionIfWhere / ExpressionWhere / ExpressionIf / Expression") %>%
  
  add_rule("OverKeyword <- '@over'") %>%
  add_rule("Equation <- Expression Equal QualifiedExpression", act = "buildCall('dynEq', v)")

# Build structures with variables and indices


nameSplit <- function(s) {
  if (str_detect(s, "_")) {
    chunks <- s %>% str_split("_") %>% unlist
    str_c(chunks[1], "^{", str_c(tail(chunks, -1), collapse = ","), "}")
  } else {
    s
  }
}

latex <- list(
  dynName = function(s) {
    s %>%
      # Sigma for elasticities
      str_replace("^(ES|es|Es|eS)_(.*)", "\\\\sigma^{\\2}") %>%
      # Phi for shares
      str_replace("^(p|P)hi_(.*)", "\\\\varphi^{\\2}") %>%
      str_replace("^(p|P)hi$", "\\\\varphi") %>%
      # Rho
      str_replace("^RHO(.*)", "\\\\rho\\1") %>%
      # Alpha for adjustment coefficients
      str_replace("^ADJUST([0-9])", "\\\\alpha_{\\1}") %>%
      str_replace("^ADJUST", "\\\\alpha") %>%
      # Mu for markup
      str_replace("MARKUP", "\\\\mu") %>%
      # Small delta for depreciation rate
      str_replace("Rdep", "\\\\delta") %>%
      # Bis and ter must appear as exponents
      str_replace("bis$", "_bis") %>%
      str_replace("ter$", "_ter") %>%
      # If underscores in name, split to exponent
      nameSplit()
  },
  dynNum = function(x) x,
  dynVar = function(name, indices = NULL, timeIndex = NULL) {
    if (is.null(indices) & is.null(timeIndex)) name
    else str_c(name, "_{", str_c(c(indices, timeIndex), collapse = ", "), "}")
  },
  dynExp = function(lst, indices, where, condition, parentheses) {
    if (parentheses) {
      str_c("\\left( ", lst, " \\right)")
    } else {
      lst
    }
  },
  dynFun = function(name, ...) {
    .args <- list(...)
    if (name == "sum") {
      str_c("\\sum_{", str_c(dynWhere(.args[[1]]), collapse = ", "), "} ", .args[[1]])
    } else if (name == "d") {
      str_c("\\varDelta \\left(", .args[[1]], "\\right)")
    } else if (name == "@elem") {
      if (str_detect(.args[[1]], ", t-1\\}")) {
        str_replace(.args[[1]], ", t-1\\}", ", t_{0}-1}")
      } else {
        str_replace(.args[[1]], "\\}$", ", t_{0}}")
      }
    } else {
      str_c("\\operatorname{", name, "} ", 
            ifelse(str_detect("args", "\\+|-|\\*|/"), 
                   str_c("\\left(", .args, "\\right)"),
                   .args))
    }
  },
  dynOp  = function(op, e1, e2) {
    # Unary operator
    if (e1 == "0") {
      str_c(op, e2)
      # Fraction
    } else if (op == "/") {
      str_c("\\frac{", e1, "}{", e2, "}")
      # Multiplication where a dot is needed
    } else if ((op == "*") & (!str_detect(e1, "_\\{"))) { #} & (!str_detect(e1, "left\\())")) & (!str_detect(e2, "right\\)"))) {
      str_c(e1, " . ", e2)
      # Generic case
    } else {
      str_c(e1, " ", ifelse(op == "*", "\\;", op), " ", e2)
    }
  },
  dynEq  = function(lhs, rhs) str_c("\\begin{dmath}\n", lhs, " = ", rhs, "\n\\end{dmath}")
)

custom <- latex



texHeader <- "\\documentclass[12pt]{article}
\\usepackage{amsmath}
\\usepackage{breqn}
\\numberwithin{equation}{section}
\\usepackage{longtable}
\\usepackage{booktabs}
\\begin{document}

"

texFooter <- "
\\end{document}"

writeFile <- function(s, filename) {
  fileConn <- file(filename)
  writeLines(s, fileConn)
  close(fileConn)
}

dependentVar <- function(eq) {
  terms <- str_replace_all(eq, "\\(|\\)|d|log", "") %>% str_split("\\+|\\*|-|=") %>% unlist %>% str_trim
  if (terms[1] == "1") terms[2] else terms[1]
}

variableTeX <- function(v) {
  code <- peg %>% apply_rule("Variable", v, exe = T) %>% value %>% parse(text = .)
  str_c("$", eval(code, latex), "$")
}

toTeX <- function(eq) {
  # !!! HACK
  # Temporary fix
  eq <- str_replace(eq, " where f in %list_F \\\\ K", "")
  
  code <- peg %>% apply_rule("Equation", eq, exe = T) %>% value %>% parse(text = .)
  eval(code, latex)
}

glossaryTeX <- function(glossary) {
  glossary <- glossary[order(tolower(names(glossary)))]
  str_c("\\newpage
        \\Large\\noindent\\textbf{Glossary}
        \\normalsize
        \\begin{longtable}{@{}p{4cm}p{9cm}@{}} \n",
        str_c(sapply(names(glossary), variableTeX), " & ", glossary, " \\\\", collapse = "\n \\midrule \n"),
        "\n\\end{longtable}")
}

readFile <- function(filename) {
  fileConn <- file(filename)
  lines <- readLines(fileConn)
  close(fileConn)
  lines
}

cleanTeX <- function(s) {
  s %>% str_replace_all(" & ", " \\\\& ")
}

codeToTeX <- function(filename) {
  # Statefulness: need to keep track of text blocks
  # to identify the title description of each variable
  description <- ""
  
  readFile(filename) %>% Reduce(function(out, l) {
    # Remove leading and trailing spaces
    l <- str_trim(l)
    
    # Section header
    processed <- if (str_detect(l, "^##### ")) {
      description <<- ""
      str_c("\n\n", "\\section{", str_replace(cleanTeX(l), "^##### ", ""), "}", "\n\n")
    } 
    # Subsection header
    else if (str_detect(l, "^#### ")) {
      description <<- ""
      str_c("\n\n", "\\subsection{", str_replace(cleanTeX(l), "^#### ", ""), "}", "\n\n")
    }
    # Subsubsection header
    else if (str_detect(l, "^### ")) {
      description <<- ""
      str_c("\n\n", "\\subsubsection{", str_replace(cleanTeX(l), "^### ", ""), "}", "\n\n")
    }
    # Blank line
    else if (l == "##") {
      "\n\n"
    }
    # Variable description
    else if (str_detect(l, "^##! ")) {
      txt <- str_replace(cleanTeX(l), "^##! ", "")
      description <<- txt 
      str_c("\\noindent\\textbf{", description, "} \\\\")
    # Pure text
    } else if (str_detect(l, "^## ")) {
      str_replace(cleanTeX(l), "^## ", "")
    # If the line is not a comment nor empty
    } else if (!str_detect(l, "^#") & (str_length(l) > 0)) {
      out$glossary[[dependentVar(l)]] <- description
      # Description title is consumed by the equation, reset it to blank
      description <<- ""
      toTeX(l)
    }
    out$code <- str_c(out$code, "\n", processed)
    out
  }, ., list(code = "", glossary = list()))
}

exportLateX <- function(filename, teXcode) {
  writeFile(str_c(texHeader, teXcode, texFooter), filename)
  texi2pdf(filename)
}

teXdoc <- function(sources, out = "doc") {
  base.path <- "../src/model/"
  compiled <- Reduce(function(out, f) {
    processed <- codeToTeX(f)
    out$code <- str_c(out$code, processed$code)
    out$glossary <- c(out$glossary, processed$glossary)
    out
  }, 
  str_c(base.path, sources), 
  list(code = "", glossary = list()))
  exportLateX(str_c(out, ".tex"), 
              str_c(compiled$code, "\n",
                    glossaryTeX(compiled$glossary)))
}