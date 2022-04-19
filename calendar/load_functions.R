gen_session_details <- function(title){
  
  out <- list(
    "4_4min_HIT" = data.frame(
      title = "4_4min_HIT",
      duration = 60,
      type = "HIT_EB",
      description = "Vortag: Kohlenhydratspeicher im Vorfeld auffüllen.
                   Verpflegung: Kohlenhydratreich.
                   Ablauf: 10min Aufwärmen per Stufen, 4min @300W, 2min Pause @140W. Wiederhole 4 Mal." 
    ),
    "3_13_30_15_HIT" = data.frame(
      title = "3_13_30_15_HIT",
      duration = 60,
      type = "HIT_IB",
      description = "Vortag: Kohlenhydratspeicher im Vorfeld auffüllen.
                   Verpflegung: Kohlenhydratreich.
                   Ablauf: 10min Aufwärmen per Stufen, 30sec @330W, 15sec Pause @140W. Wiederhole 13 Mal." 
    ),
    "2h_LIT" = data.frame(
      title = "2h_LIT",
      type = "LIT",
      duration = 120,
      description = "Ggf. ersten 60min nüchtern. Ggf. Koffeein/Teein zuführen. Slow Carb zuführen." 
    )
  )
  
  out[[title]]
  
}
