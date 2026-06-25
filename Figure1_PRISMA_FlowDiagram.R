# Figure 1 — PRISMA 2020 flow diagram
# Matches the navy figure family of the manuscript (cf. Figure2_Forest_Plots_JAMA.R).
# Counts reconciled to the Rayyan screening record (primary source):
#   371 identified -> 59 duplicates -> 312 screened -> 297 excluded -> 15 included in review and synthesis.
# Square-cornered boxes follow the canonical PRISMA layout. To round the corners,
# add library(ggchicklet) and swap geom_rect() for geom_rrect().

suppressMessages({library(ggplot2); library(grid)})

navy <- "#003366"; ink <- "#1f2a36"; gray <- "#5d6d7e"
bdr  <- "#9fb0c0"; excl_fill <- "#f7f9fb"; band <- "#eef2f6"
FONT <- "sans"

## ---- box geometry on a 0-100 x 0-100 canvas ----
rects <- data.frame(
  id   = c("idn","dup","scr","exc","inc"),
  cx   = c(36,   80,   36,   80,   36),
  cy   = c(90,   90,   58,   58,   17),
  w    = c(40,   34,   40,   38,   46),
  h    = c(11,   9,    11,   25,   13),
  fill = c("white","white","white",excl_fill,"white"),
  stringsAsFactors = FALSE
)
rects$xmin <- rects$cx - rects$w/2; rects$xmax <- rects$cx + rects$w/2
rects$ymin <- rects$cy - rects$h/2; rects$ymax <- rects$cy + rects$h/2
g <- function(i) rects[rects$id == i, ]

bands <- data.frame(
  xmin = 1.5, xmax = 9,
  ymin = c(70, 34, 1), ymax = c(99, 69, 33),
  lab  = c("Identification","Screening","Included"),
  ly   = c(84.5, 51.5, 17)
)

p <- ggplot() +
  geom_rect(data = bands, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = band, color = NA) +
  geom_text(data = bands, aes(x = (xmin + xmax)/2, y = ly, label = lab),
            angle = 90, fontface = "bold", color = navy, size = 4.1, family = FONT) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            color = bdr, linewidth = 0.5) +
  scale_fill_identity()

## ---- centered boxes (bold title + body line) ----
add_centered <- function(p, r, title, body, ts = 3.5, bs = 3.2) {
  p +
    annotate("text", x = r$cx, y = r$cy + 0.17 * r$h, label = title,
             fontface = "bold", color = navy, size = ts, family = FONT) +
    annotate("text", x = r$cx, y = r$cy - 0.20 * r$h, label = body,
             color = ink, size = bs, family = FONT)
}
p <- add_centered(p, g("idn"), "Records identified from databases", "(n = 371)")
p <- add_centered(p, g("dup"), "Duplicate records removed", "before screening (n = 59)", ts = 3.3, bs = 3.1)
p <- add_centered(p, g("scr"), "Records screened", "(n = 312)")
p <- add_centered(p, g("inc"), "Studies included in review and", "synthesis (n = 15)")

## ---- exclusion boxes (centered bold header + left-aligned body) ----
exc <- g("exc")
p <- p +
  annotate("text", x = exc$cx, y = exc$ymax - 3, label = "Records excluded (n = 297)",
           fontface = "bold", color = navy, size = 3.4, family = FONT) +
  annotate("text", x = exc$xmin + 2.5, y = exc$ymax - 6.5, hjust = 0, vjust = 1,
           label = paste("Non-eligible study designs   n = 138",
                         "Inadequate outcome   n = 71",
                         "Non-eligible exposures   n = 49",
                         "Non-human population   n = 28",
                         "Exposure outside the",
                         "gestational window   n = 11", sep = "\n"),
           color = ink, size = 3.1, family = FONT, lineheight = 1.25)

## ---- arrows ----
ar  <- arrow(length = unit(0.18, "cm"), type = "closed")
seg <- function(p, x, xe, y, ye)
  p + annotate("segment", x = x, xend = xe, y = y, yend = ye,
               arrow = ar, color = gray, linewidth = 0.55)
# vertical (down the main column)
p <- seg(p, 36, 36, 84.5, 63.6)   # identified  -> screened
p <- seg(p, 36, 36, 52.5, 23.7)   # screened    -> included
# horizontal (out to the exclusion boxes)
p <- seg(p, 56, 62.5, 90, 90)     # identified  -> duplicates removed
p <- seg(p, 56, 60.5, 58, 58)     # screened    -> records excluded

p <- p +
  coord_cartesian(xlim = c(0, 100), ylim = c(-2, 100), clip = "off") +
  labs(caption = "Flow diagram prepared according to the PRISMA 2020 statement.") +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0.5, size = 7.5, color = gray,
                                    face = "italic", family = FONT, margin = margin(t = 6)),
        plot.margin = margin(6, 8, 6, 6))

ggsave("Figure1_PRISMA_FlowDiagram.pdf", p, width = 8.8, height = 9.8, device = cairo_pdf)  # vector: preferred for line art
ggsave("Figure1_PRISMA_FlowDiagram.png", p, width = 8.8, height = 9.8, dpi = 600, bg = "white")  # 600 dpi raster (JAMA min. 350)
cat("Figure 1 rendered.\n")
