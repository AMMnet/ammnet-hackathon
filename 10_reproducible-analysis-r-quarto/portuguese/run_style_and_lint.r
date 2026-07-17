## run_style_and_lint.r
## Formatar (style) e auditar (lint) automaticamente os scripts de R neste projecto.

# Carregar pacotes
library(here)
library(styler)
library(lintr)

# Definir directórios para verificar
target_dirs <- c(
  here::here("scripts")
)

# 1. Executar Styler (Formatação) --------------------------------------------
cat("========================================================================\n")
cat("1. FORMATANDO FICHEIROS DE CÓDIGO R (styler)\n")
cat("========================================================================\n")

for (dir in target_dirs) {
  if (dir.exists(dir)) {
    cat("Formatando o directório:", basename(dir), "\n")
    styler::style_dir(dir)
  } else {
    cat("Directório não encontrado:", dir, "\n")
  }
}
cat("\n")

# 2. Executar Lintr (Auditoria de Estilo e Sintaxe) --------------------------
cat("========================================================================\n")
cat("2. AUDITANDO FICHEIROS DE CÓDIGO R (lintr)\n")
cat("========================================================================\n")

any_lints <- FALSE
for (dir in target_dirs) {
  if (dir.exists(dir)) {
    cat("Auditando o directório:", basename(dir), "\n")
    lint_results <- lintr::lint_dir(dir)

    if (length(lint_results) > 0) {
      any_lints <- TRUE
      print(lint_results)
    } else {
      cat("✔ Nenhum aviso de estilo/sintaxe encontrado em:", basename(dir), "\n")
    }
  }
}

cat("========================================================================\n")
if (any_lints) {
  cat("Estado: Concluído. Por favor, reveja e corrija os problemas acima.\n")
} else {
  cat("Estado: Sucesso! Todos os scripts estão limpos e devidamente formatados.\n")
}
cat("========================================================================\n")
