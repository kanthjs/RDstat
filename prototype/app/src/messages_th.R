# Thai wording of the messages the analysis code (star/star_*.R) gives in English: data checks, layout problems and the
# reasons an analysis stops. The engine keeps English (its tests, the R scripts and the Word / Excel reports use it);
# the screen shows the Thai text with the English original beneath it. Each entry is a perl regex matched anywhere in the
# message and its replacement, so a message built from several sentences is translated sentence by sentence and one
# without a pattern stays in English. Keep a pattern in step with its message when the engine's wording changes
# (test_server.R checks that every message the layout tests raise translates fully).
msg_th_rules <- c(
  # ---- reading the file ----
  "The file is empty\\." = "ไฟล์ว่างเปล่า",
  "The file needs a header row, at least two columns and one data row\\." = "ไฟล์ต้องมีแถวชื่อคอลัมน์ อย่างน้อย 2 คอลัมน์ และข้อมูลอย่างน้อย 1 แถว",
  "Column names must be non-empty and unique\\." = "ชื่อคอลัมน์ต้องไม่ว่างและไม่ซ้ำกัน",
  "Repeated the label of (\\d+) merged cells? on every row (?:they|it) covers? \\((\\d+) cells? filled in\\)\\." =
    "เติมป้ายชื่อของเซลล์ที่ผสาน (merge) \\1 กลุ่มลงในทุกแถวที่เซลล์นั้นครอบคลุม (เติม \\2 เซลล์)",
  "Column (.+?): each label was typed once for (\\d+) rows \\(cells merged in Excel\\?\\), so it was repeated on the (\\d+) blank rows below it\\." =
    "คอลัมน์ \\1: ป้ายชื่อพิมพ์ไว้ครั้งเดียวต่อ \\2 แถว (อาจผสานเซลล์ใน Excel) จึงเติมลงในแถวว่างด้านล่าง \\3 แถว",
  "Skipped (\\d+) lines? above the column names: (.*)\\.$" = "ข้ามข้อความ \\1 บรรทัดที่อยู่เหนือชื่อคอลัมน์: \\2",
  "Left out (\\d+) columns? without a name holding only notes: (.*)\\.$" = "ไม่นำ \\1 คอลัมน์ที่ไม่มีชื่อและมีแต่หมายเหตุมาวิเคราะห์: \\2",
  "Skipped (\\d+) summary rows? below the data: (.*)\\.$" = "ข้ามแถวสรุป \\1 แถวที่อยู่ใต้ข้อมูล: \\2",

  # ---- the response and the label columns ----
  "The data set has no rows\\." = "ข้อมูลไม่มีแถวเลย",
  "The response '([^']*)' contains no numbers\\. Choose a numeric column\\." = "คอลัมน์ผลตอบสนอง '\\1' ไม่มีค่าที่เป็นตัวเลข โปรดเลือกคอลัมน์ที่เป็นตัวเลข",
  "The response '([^']*)' is mostly text \\((\\d+) of (\\d+) entries are not numbers, e\\.g\\. (.*?)\\)\\. Choose a numeric column\\." =
    "คอลัมน์ผลตอบสนอง '\\1' ส่วนใหญ่เป็นข้อความ (\\2 จาก \\3 ค่าไม่ใช่ตัวเลข เช่น \\4) โปรดเลือกคอลัมน์ที่เป็นตัวเลข",
  "Response '([^']*)': (\\d+) text value\\(s\\) were read as numbers \\(e\\.g\\. (.*?)\\)\\.(?= |$)" = "ผลตอบสนอง '\\1': อ่านค่าที่เก็บเป็นข้อความให้เป็นตัวเลข \\2 ค่า (เช่น \\3)",
  "Response '([^']*)': (\\d+) value\\(s\\) are not numbers and were treated as missing \\(e\\.g\\. (.*?), row (\\d+)\\)\\." =
    "ผลตอบสนอง '\\1': มี \\2 ค่าที่ไม่ใช่ตัวเลข จึงถือเป็นค่าว่าง (เช่น \\3 แถวที่ \\4)",
  "Response '([^']*)': (\\d+) infinite value\\(s\\) were treated as missing\\." = "ผลตอบสนอง '\\1': ค่าอนันต์ \\2 ค่าถือเป็นค่าว่าง",
  "Column '([^']*)': extra spaces were removed from (\\d+) label\\(s\\)\\." = "คอลัมน์ '\\1': ลบช่องว่างที่เกินออกจากป้ายชื่อ \\2 ค่า",
  "Column '([^']*)': labels that differ only in upper/lower case are treated as different levels \\((.*?)\\)\\. Check for typing errors\\." =
    "คอลัมน์ '\\1': ป้ายชื่อที่ต่างกันแค่ตัวพิมพ์ใหญ่/เล็กถูกนับเป็นคนละระดับ (\\2) โปรดตรวจว่าพิมพ์ผิดหรือไม่",
  "Column '([^']*)': (\\d+) row\\(s\\) have no label\\." = "คอลัมน์ '\\1': มี \\2 แถวที่ไม่มีป้ายชื่อ",
  "(\\d+) row\\(s\\) with a missing treatment or block label were removed \\(row (.*?)\\)\\.(?= |$)" = "ตัดแถวที่ไม่มีชื่อกรรมวิธีหรือบล็อกออก \\1 แถว (แถวที่ \\2)",
  "No rows are left after removing rows with missing treatment or block labels\\." = "ไม่เหลือข้อมูลหลังตัดแถวที่ไม่มีชื่อกรรมวิธีหรือบล็อกออก",
  "Response '([^']*)' has (\\d+) missing value\\(s\\) out of (\\d+) rows\\." = "ผลตอบสนอง '\\1' มีค่าว่าง \\2 ค่า จากทั้งหมด \\3 แถว",
  "The response '([^']*)' needs at least 3 non-missing values\\." = "ผลตอบสนอง '\\1' ต้องมีค่าที่ไม่ว่างอย่างน้อย 3 ค่า",
  "The response '([^']*)' is constant; there is nothing to analyse\\." = "ผลตอบสนอง '\\1' มีค่าเท่ากันทุกแถว จึงไม่มีอะไรให้วิเคราะห์",
  "The factor '([^']*)' needs at least two levels \\(it has (\\d+)\\)\\." = "ปัจจัย '\\1' ต้องมีอย่างน้อย 2 ระดับ (ตอนนี้มี \\2 ระดับ)",
  "The factor '([^']*)' has a different level on every row \\((\\d+) levels\\); it looks like an ID or a continuous measurement, not a treatment\\." =
    "ปัจจัย '\\1' มีค่าไม่ซ้ำกันเลยทุกแถว (\\2 ระดับ) ดูเหมือนเป็นรหัสหรือค่าที่วัดได้ ไม่ใช่กรรมวิธี",
  "Column '([^']*)' has (\\d+) levels for (\\d+) rows; check that this is a treatment factor\\." = "คอลัมน์ '\\1' มี \\2 ระดับจากข้อมูล \\3 แถว โปรดตรวจว่าเป็นคอลัมน์กรรมวิธีจริง",
  "The block column '([^']*)' needs at least two levels\\." = "คอลัมน์บล็อก '\\1' ต้องมีอย่างน้อย 2 ระดับ",
  "(\\d+) of (\\d+) rows are used in the analysis\\." = "ใช้ข้อมูลในการวิเคราะห์ \\1 จาก \\2 แถว",

  # ---- analysis of variance ----
  "Choose at least one treatment factor\\." = "โปรดเลือกปัจจัยกรรมวิธีอย่างน้อย 1 ปัจจัย",
  "Only one or two treatment factors are supported so far\\." = "ขณะนี้รองรับปัจจัยกรรมวิธีได้ 1 หรือ 2 ปัจจัยเท่านั้น",
  "An RCBD analysis needs a block column\\." = "การวิเคราะห์ RCBD ต้องมีคอลัมน์บล็อก",
  "Some treatment combinations have no observations; the design must be complete\\." = "บางชุดกรรมวิธีไม่มีข้อมูลเลย แผนการทดลองต้องมีครบทุกชุด",
  "The (\\d+) row\\(s\\) with a missing response were left out of the analysis\\." = "ไม่นำ \\1 แถวที่ไม่มีค่าผลตอบสนองมาวิเคราะห์",
  "(\\d+) missing response value\\(s\\) were estimated by least squares \\(Yates' missing-plot method: the values predicted by lm\\(\\) fitted to the observed data\\); the error degrees of freedom are reduced by (\\d+)\\." =
    "ประมาณค่าแปลงที่หาย \\1 ค่าด้วยวิธีกำลังสองน้อยสุด (วิธีของ Yates: ค่าทำนายจาก lm() ที่ฟิตกับข้อมูลที่มีอยู่) และลดองศาอิสระของความคลาดเคลื่อนลง \\2",
  "There are no error degrees of freedom: each treatment needs more than one plot \\(replication\\) to test differences\\." =
    "ไม่มีองศาอิสระของความคลาดเคลื่อน แต่ละกรรมวิธีต้องมีมากกว่า 1 แปลง (ซ้ำ) จึงจะทดสอบความแตกต่างได้",
  "The error variance is zero: every plot equals the mean of its treatment, so F and P cannot be computed\\. Check that the response column holds the plot values and not treatment means repeated on each row\\." =
    "ความแปรปรวนของความคลาดเคลื่อนเป็นศูนย์ ทุกแปลงมีค่าเท่ากับค่าเฉลี่ยของกรรมวิธีพอดี จึงคำนวณ F และ P ไม่ได้ โปรดตรวจว่าคอลัมน์ผลตอบสนองเป็นค่ารายแปลง ไม่ใช่ค่าเฉลี่ยของกรรมวิธีที่ใส่ซ้ำทุกแถว",
  "(\\d+) treatment\\(s\\) have a single observation \\((.*?)\\); their means have no replication and the comparisons involving them are weak\\." =
    "มี \\1 กรรมวิธีที่มีข้อมูลเพียงค่าเดียว (\\2) ค่าเฉลี่ยของกรรมวิธีนั้นจึงไม่มีซ้ำ และการเปรียบเทียบที่เกี่ยวข้องมีความน่าเชื่อถือต่ำ",
  "The data are unbalanced: an RCBD needs one plot per treatment in each block\\." = "ข้อมูลไม่สมดุล: RCBD ต้องมีกรรมวิธีละ 1 แปลงในทุกบล็อก",
  "Too many missing values: (\\d+) of (\\d+) plots \\(more than 10%\\) have no response\\. Missing values can be estimated for up to 10% of the plots\\." =
    "ค่าที่หายไปมากเกินไป: \\1 จาก \\2 แปลง (เกิน 10%) ไม่มีค่าผลตอบสนอง ซึ่งประมาณค่าแทนได้ไม่เกิน 10% ของจำนวนแปลง",
  "(\\d+) of them have no row in the data\\." = "ในจำนวนนี้มี \\1 แปลงที่ไม่มีแถวอยู่ในข้อมูลเลย",
  "The layout is not complete: an RCBD needs every treatment in every block\\." = "ผังข้อมูลไม่ครบ: RCBD ต้องมีครบทุกกรรมวิธีในทุกบล็อก",
  " ?\\(Plots with no row are treated as missing, and at most 10% can be estimated; here (\\d+) of (\\d+)\\.\\)" =
    " (แปลงที่ไม่มีแถวในข้อมูลจะถือเป็นค่าที่หาย ซึ่งประมาณค่าแทนได้ไม่เกิน 10% ในข้อมูลนี้มี \\1 จาก \\2 แปลง)",
  "The missing values cannot be estimated: a treatment or block has too few observed plots\\." = "ประมาณค่าที่หายไปไม่ได้ เพราะบางกรรมวิธีหรือบล็อกมีแปลงที่มีข้อมูลน้อยเกินไป",

  # ---- layout problems (.star_layout_problem) ----
  "(\\d+) combination\\(s\\) have no row: (.*?)\\.(?= |$)" = "มี \\1 ชุดที่ไม่มีข้อมูล: \\2",
  "(\\d+) combination\\(s\\) have more than one row: (.*?)\\.(?= |$)" = "มี \\1 ชุดที่มีข้อมูลมากกว่า 1 แถว: \\2",
  "; and (\\d+) more" = "; และอีก \\1 ชุด",
  "In ([^,]+?), (.+?) appear\\(s\\) with only some levels of (.+?): check these labels for a typing error\\." =
    "ในคอลัมน์ \\1 ป้ายชื่อ \\2 พบเพียงในบางระดับของ \\3 โปรดตรวจว่าพิมพ์ชื่อผิดหรือไม่",
  "No level of (.+?) appears with every level of (.+?): check that the right columns are chosen \\(in a complete layout every block holds every treatment\\)\\." =
    "ไม่มีระดับใดของ \\1 ที่พบครบกับทุกระดับของ \\2 โปรดตรวจว่าเลือกคอลัมน์ถูกต้อง (ในแผนที่สมบูรณ์ ทุกบล็อกต้องมีครบทุกกรรมวิธี)",
  "In ([^,]+?), (.+?) differ only in upper/lower case and are read as different labels\\." =
    "ในคอลัมน์ \\1 ป้ายชื่อ \\2 ต่างกันแค่ตัวพิมพ์ใหญ่/เล็ก จึงถูกนับเป็นคนละระดับ",
  "' and '" = "' กับ '",

  # ---- split plot ----
  "A split-plot analysis needs exactly one main-plot factor and one sub-plot factor\\." = "การวิเคราะห์ split plot ต้องมีปัจจัยแปลงหลัก 1 ปัจจัย และปัจจัยแปลงย่อย 1 ปัจจัย",
  "A split plot in CRD needs a replication column \\(the number of each main plot within its level of the main-plot factor\\)\\." =
    "split plot ใน CRD ต้องมีคอลัมน์ซ้ำ (หมายเลขของแปลงหลักแต่ละแปลงภายในระดับของปัจจัยแปลงหลัก)",
  "A split-plot analysis needs a block \\(replication\\) column\\." = "การวิเคราะห์ split plot ต้องมีคอลัมน์บล็อก (ซ้ำ)",
  "The response has (\\d+) missing value\\(s\\); the split-plot analysis needs a complete, balanced layout\\. Remove or fill them in and try again\\." =
    "ผลตอบสนองมีค่าว่าง \\1 ค่า การวิเคราะห์ split plot ต้องมีข้อมูลครบและสมดุล โปรดตัดแถวนั้นออกหรือเติมค่าแล้วลองใหม่",
  "The layout is not complete and balanced: each (.+?) combination must appear exactly once\\." = "ผังข้อมูลไม่ครบหรือไม่สมดุล: แต่ละชุด \\1 ต้องมีข้อมูลพอดี 1 แถว",
  "Could not fit this split-plot model\\." = "ฟิตโมเดล split plot นี้ไม่ได้",

  # ---- combined analysis ----
  "A combined analysis needs exactly one treatment factor\\." = "การวิเคราะห์รวมต้องมีปัจจัยกรรมวิธี 1 ปัจจัยพอดี",
  "A combined analysis needs an environment column \\(site, season or year\\)\\." = "การวิเคราะห์รวมต้องมีคอลัมน์สภาพแวดล้อม (สถานที่ ฤดู หรือปี)",
  "A combined RCBD analysis needs a block \\(replication\\) column\\." = "การวิเคราะห์รวมแบบ RCBD ต้องมีคอลัมน์บล็อก (ซ้ำ)",
  "The environment column '([^']*)' needs at least two levels\\." = "คอลัมน์สภาพแวดล้อม '\\1' ต้องมีอย่างน้อย 2 ระดับ",
  "Every treatment must appear in every environment\\." = "ทุกกรรมวิธีต้องมีข้อมูลในทุกสภาพแวดล้อม",
  "Some are missing \\(a missing yield also counts\\); estimate or remove them and try again\\." = "บางชุดขาดหายไป (ค่าผลผลิตที่ว่างก็นับด้วย) โปรดประมาณค่าหรือตัดออกแล้วลองใหม่",
  "Could not fit the combined model\\." = "ฟิตโมเดลการวิเคราะห์รวมไม่ได้",

  # ---- repeated measures ----
  "A repeated-measures analysis needs exactly one treatment factor and one time factor\\." = "การวิเคราะห์วัดซ้ำต้องมีปัจจัยกรรมวิธี 1 ปัจจัย และปัจจัยเวลา 1 ปัจจัย",
  "A repeated-measures analysis in CRD needs a plot column \\(the number of each plot within its treatment: 1, 2, 3, \\.\\.\\.\\)\\." =
    "การวิเคราะห์วัดซ้ำใน CRD ต้องมีคอลัมน์แปลง (หมายเลขแปลงภายในแต่ละกรรมวิธี: 1, 2, 3, ...)",
  "A repeated-measures analysis in RCBD needs a block \\(replication\\) column\\." = "การวิเคราะห์วัดซ้ำใน RCBD ต้องมีคอลัมน์บล็อก (ซ้ำ)",
  "The response has (\\d+) missing value\\(s\\); the repeated-measures analysis needs every plot measured at every time\\. Remove the plots with missing values or fill them in and try again\\." =
    "ผลตอบสนองมีค่าว่าง \\1 ค่า การวิเคราะห์วัดซ้ำต้องมีข้อมูลของทุกแปลงในทุกครั้งที่วัด โปรดตัดแปลงที่มีค่าว่างออกหรือเติมค่าแล้วลองใหม่",
  "The time factor needs at least two times\\." = "ปัจจัยเวลาต้องมีอย่างน้อย 2 ครั้ง",
  "The layout is not complete: each plot \\((.+?)\\) must have exactly one value at each level of (.+?)\\.(?= |$)" = "ผังข้อมูลไม่ครบ: แต่ละแปลง (\\1) ต้องมีค่าพอดี 1 ค่าในแต่ละระดับของ \\2",

  # ---- prefixes the screens put before an engine message ----
  "^Welch's ANOVA could not be run: " = "วิเคราะห์ Welch's ANOVA ไม่ได้: ",
  "^The trimmed-means comparison could not be run: " = "เปรียบเทียบค่าเฉลี่ยตัดปลายไม่ได้: ",
  "^The rank-based test could not be run: " = "ทดสอบแบบอันดับไม่ได้: ",
  "^The trimmed-means test could not be run: " = "ทดสอบค่าเฉลี่ยตัดปลายไม่ได้: "
)

# the Thai wording of one message; the message itself when no pattern applies
msg_th <- function(msg) {
  out <- msg
  for (i in seq_along(msg_th_rules)) out <- gsub(names(msg_th_rules)[i], msg_th_rules[[i]], out, perl = TRUE)
  out
}
# a message for the screen: the Thai wording, with the English original in small grey text when they differ
msg_ui <- function(msg) {
  if (!is.character(msg) || length(msg) != 1) return(msg) # already a tag
  th <- msg_th(msg)
  if (identical(th, msg)) return(msg)
  tagList(th, div(class = "small text-muted mt-1", lang = "en", msg))
}
