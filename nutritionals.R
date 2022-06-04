nutritionals <- data.frame(
  name = character(0),
  kcal = integer(0),
  fat = integer(0),
  carbs = integer(0),
  proteins = integer(0),
  gram = integer(0)
)

nutritionals[1, ] <- c("banana (120g)", 115.2, 0.2, 26.4, 1.2, 120)
nutritionals[2, ] <- c("bun (50g)", 132, 0.5, 26.1, 4.6, 50)
nutritionals[3, ] <- c("apple (125g)", 65, 0.5, 14.3, 0.4, 125)
nutritionals[4, ] <- c("raisins (10g)", 33, 0.11, 7.4, 0.27, 10)
nutritionals[5, ] <- c("haribo Matador(100g)", 340, 0.5, 82, 2, 100)
nutritionals[5, ] <- c("Maltodextrin (30g) - scoop", 33, 0, 28.5, 0, 30)