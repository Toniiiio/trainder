gen_session_details <- function(title){
  
create_eb_descr <- function(watt_intense, dur_intense, watt_break,  break_length, amt_rep){
    paste0(
"Vortag: Kohlenhydratspeicher im Vorfeld auffüllen.
Verpflegung: Kohlenhydratreich.
Ablauf: 10min Aufwärmen per Stufen, ", dur_intense,"min @ ", watt_intense,"Watt, ", break_length,"min Pause @ ", watt_break," Watt. Wiederhole ", amt_rep," Mal."
    )
  }
  
  
  out <- list(
    "4_4min_HIT" = data.frame(
      title = "4_4min_HIT",
      duration = 60,
      type = "HIT",
      description = create_eb_descr(watt_intense = 300, 5, 130,  2, 5) 
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
