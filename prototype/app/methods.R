# Text of the Methods page: what each analysis does, the functions that do it and the references, by topic. The
# software references are built from the installed packages (package_ref). Keep this in step with the engine code:
# engine/R/star_aov.R, star_splitplot.R, star_helpers.R, star_diagnostics.R, star_ttest.R, star_combined.R, star_nonparametric.R, star_correlation.R, star_regression.R.

# the groups of topics, in the order shown
method_groups <- c(
  two = "เปรียบเทียบสองกลุ่ม",
  aov = "ANOVA ตามแผนการทดลอง",
  after = "หลังการวิเคราะห์ความแปรปรวน",
  alt = "เมื่อข้อมูลไม่เป็นไปตามข้อตกลงเบื้องต้น",
  corr = "สหสัมพันธ์และการถดถอย",
  data = "ข้อมูล"
)
# One topic of the Methods page: what the app does (Thai text in ...), the functions that do it ("pkg::fun()"; packages
# other than base R are listed with their version) and the keys of its references in method_refs (start of the text).
method_topic <- function(
  id,
  title,
  group,
  ...,
  software = character(),
  refs = character()
) {
  list(
    id = id,
    title = title,
    group = group,
    body = tagList(...),
    software = software,
    refs = refs
  )
}
method_topics <- list(
  method_topic(
    "ttest",
    "t-test",
    "two",
    software = c(
      "stats::t.test()",
      "stats::var.test()",
      "stats::shapiro.test()"
    ),
    refs = c(
      "Student (1908)",
      "Welch BL (1947)",
      "Satterthwaite FE (1946)",
      "Cohen J (1988)",
      "Shapiro SS, Wilk MB (1965)"
    ),
    p(
      "ใช้ ",
      code("t.test()"),
      " สองกลุ่มอิสระ: แสดงทั้ง t-test แบบสมมติความแปรปรวนเท่ากัน (Student’s pooled-variance t-test; Student, 1908) และ t-test แบบไม่สมมติความแปรปรวนเท่ากัน (Welch’s t-test; Welch, 1947) โดย Welch’s t-test ใช้การประมาณองศาอิสระของ Satterthwaite (Satterthwaite, 1946) เช่นเดียวกับโปรแกรม STAR ",
      "ผู้ใช้เลือกได้ว่าจะสรุปผลจากแบบใด ค่าเริ่มต้นคือ pooled-variance t-test หากความแปรปรวนของสองกลุ่มแตกต่างกัน แอปจะแนะนำให้ใช้ Welch’s t-test (ดูหัวข้อค่าเริ่มต้น) การทดสอบความเท่ากันของความแปรปรวนด้วย F-test (",
      code("var.test()"),
      ") แสดงเป็นข้อมูลประกอบเท่านั้น ไม่ใช้เลือกวิธี ",
      "แบบจับคู่ (paired): ทดสอบค่าเฉลี่ยของผลต่างรายหน่วย โดยผลต่างคือค่าการวัดครั้งแรกลบค่าการวัดครั้งที่สอง และใช้เฉพาะคู่ข้อมูลที่มีค่าครบทั้งสองครั้ง ",
      "แบบกลุ่มเดียว (one sample): เปรียบเทียบค่าเฉลี่ยกับค่าที่กำหนด ",
      "ช่วงความเชื่อมั่นของผลต่างอยู่ที่ระดับ 1 - alpha ขนาดอิทธิพล Cohen's d (Cohen, 1988) = ผลต่างของค่าเฉลี่ย / SD รวม (สองกลุ่มอิสระ), ค่าเฉลี่ยของผลต่าง / SD ของผลต่าง (dz, แบบจับคู่) หรือ (ค่าเฉลี่ย - ค่าที่กำหนด) / SD (กลุ่มเดียว) ",
      "คำว่า เล็ก ปานกลาง และใหญ่ ใช้เกณฑ์ 0.2, 0.5 และ 0.8 ของ Cohen อย่างไรก็ตาม ควรตีความร่วมกับบริบทของงานวิจัยและความสำคัญเชิงปฏิบัติของผลต่าง ไม่ควรใช้เกณฑ์นี้แทนการตัดสินเชิงวิชาการ การตรวจสอบสมมติฐานการแจกแจงปกติด้วย Shapiro–Wilk test โดยตรวจแยกในแต่ละกลุ่มสำหรับการเปรียบเทียบสองกลุ่มอิสระ และตรวจจากผลต่างรายหน่วยสำหรับข้อมูลแบบจับคู่"
    )
  ),

  method_topic(
    "crd",
    "ANOVA : แผนการทดลองแบบสุ่มสมบูรณ์ (CRD)",
    "aov",
    software = c("stats::aov()"),
    refs = c("Chambers JM, Freeny A", "Gomez KA, Gomez AA (1984)"),
    p(
      "โมเดลแบบอิทธิพลคงที่ (fixed-effects model) สำหรับการทดลองปัจจัยเดียวคือ ",
      code("y ~ A"),
      " และสำหรับการทดลองสองปัจจัยพร้อมปฏิสัมพันธ์คือ ",
      code("y ~ A * B"),
      " โดยประมาณพารามิเตอร์ด้วยฟังก์ชัน ",
      code("aov()"),
      " (Chambers et al., 1992) ค่า F สำหรับทดสอบอิทธิพลแต่ละอย่างคำนวณจาก F = MS(อิทธิพล) / MS(error) ข้อมูลแถวที่ไม่มีค่าตัวแปรตอบสนองจะถูกตัดออกก่อนการวิเคราะห์ ",
      "ตาราง ANOVA รายงานผลบวกกำลังสองแบบลำดับ (Type I sums of squares) ตามค่าเริ่มต้นของ ",
      code("aov()"),
      " ดังนั้นในการทดลองสองปัจจัย หากจำนวนข้อมูลในแต่ละ treatment combination ไม่เท่ากัน ",
      "ผลบวกกำลังสองและการทดสอบอิทธิพลของปัจจัยที่ป้อนเข้ามาทีหลัง จะขึ้นอยู่กับลำดับของปัจจัยในสูตรโมเดล ",
      "เช่น ผลจาก ",
      code("y ~ A * B"),
      " อาจไม่เท่ากับผลจาก ",
      code("y ~ B * A")
    ),
    p(
      "ค่าสัมประสิทธิ์การแปรผันคำนวณจาก CV (%) = 100 × sqrt(MS error) / ค่าเฉลี่ยรวม ",
      "ส่วนขนาดอิทธิพลแบบ partial eta-squared คำนวณจาก SS(อิทธิพล) / [SS(อิทธิพล) + SS(error)] ",
      "ในตารางค่าเฉลี่ยรายกลุ่ม SE หมายถึงส่วนเบี่ยงเบนมาตรฐานของกลุ่มนั้น หารด้วยรากที่สองของจำนวนข้อมูลในกลุ่ม"
    ),
    p("สามารถศึกษารายละเอียดเพิ่มเติมได้จาก Gomez & Gomez (1984)")
  ),

  method_topic(
    "rcbd",
    "ANOVA: แผนการทดลองแบบสุ่มในบล็อกสมบูรณ์ (RCBD)",
    "aov",
    software = c("stats::aov()", "stats::lm()", "stats::predict()"),
    refs = c("Chambers JM, Freeny A", "Yates F (1933)", "Gomez KA, Gomez AA (1984)"),
    p(
      "โมเดลสำหรับ RCBD ปัจจัยเดียวคือ ",
      code("y ~ Block + A"),
      " และสำหรับการทดลองสองปัจจัยพร้อมปฏิสัมพันธ์คือ ",
      code("y ~ Block + A * B"),
      " โดยกำหนดให้ block เป็น fixed effect"
    ),
    p(
      "หากทุกคู่ของ block และ treatment มีจำนวนข้อมูลเท่ากันและไม่มีค่าตัวแปรตอบสนองสูญหาย ",
      "จะประมาณโมเดลนี้โดยตรงด้วย ",
      code("aov()")
    ),
    p(
      "หากมีแปลงที่ค่าหายไป โปรแกรมจะประมาณค่าที่หายไปด้วยวิธีกำลังสองน้อยที่สุดของ Yates (missing-plot technique; Yates, 1933) ",
      "คือค่าที่ทำให้ผลบวกกำลังสองของ error ต่ำที่สุด ซึ่งเท่ากับค่าทำนายจากโมเดลเดียวกันที่ fit กับข้อมูลที่มีอยู่ด้วย ",
      code("lm()"),
      " และ ",
      code("predict()"),
      " จากนั้นวิเคราะห์ข้อมูลที่เติมค่าแล้วด้วย ",
      code("aov()"),
      " และลดองศาอิสระของ error ลงเท่ากับจำนวนค่าที่ประมาณ (Gomez & Gomez, 1984)"
    ),
    p(
      "ค่าเฉลี่ยในตารางคำนวณจากข้อมูลที่มีอยู่จริงเท่านั้น ",
      "การวิเคราะห์จะไม่ดำเนินการ หากมีมากกว่าหนึ่งแถวในคู่ block × treatment เดียวกัน ",
      "หรือมีค่าหายเกิน 10% ของจำนวนแปลง ตามเงื่อนไขเดียวกับโปรแกรม STAR ",
      "ค่า F ที่ได้จากวิธีนี้อาจสูงกว่าค่าที่ได้จากการวิเคราะห์ข้อมูลไม่สมดุลโดยตรงเล็กน้อย (Yates, 1933)"
    )
  ),

  method_topic(
    "split_crd",
    "ANOVA: แผนการทดลองแบบ split plot ใน CRD",
    "aov",
    software = c("stats::aov()"),
    refs = c("Chambers JM, Freeny A", "Gomez KA, Gomez AA (1984)"),
    p(
      "โมเดล: ",
      code("y ~ A * B + Error(Rep:A)"),
      " ประมาณพารามิเตอร์ด้วย ",
      code("aov()"),
      " โดย A เป็นปัจจัยหลักที่จัดสรรแบบสุ่มสมบูรณ์ลงในแปลงใหญ่ และ B เป็นปัจจัยรองที่จัดสรรภายในแปลงใหญ่ ",
      "แผนการทดลองนี้ไม่มีบล็อก คอลัมน์ Replication จึงหมายถึงหมายเลขแปลงใหญ่ภายในแต่ละระดับของ A"
    ),
    p(
      "Error(a) คือความแปรปรวนระหว่างแปลงใหญ่ภายในแต่ละระดับของ A หรือ Rep(A) ",
      "มีองศาอิสระเท่ากับ a(r - 1) และใช้เป็น error term สำหรับทดสอบอิทธิพลของ A ",
      "ส่วน Error(b) คือความคลาดเคลื่อนระดับแปลงย่อย มีองศาอิสระเท่ากับ a(r - 1)(b - 1) ",
      "และใช้ทดสอบอิทธิพลของ B และปฏิสัมพันธ์ A × B (Gomez & Gomez, 1984)"
    ),
    p(
      "การคำนวณ CV(a), CV(b) และการเปรียบเทียบค่าเฉลี่ย ใช้หลักการเดียวกับ split plot ใน RCBD ตามหัวข้อถัดไป ",
      "ข้อมูลต้องมีจำนวนครบและสมดุล กล่าวคือ มีค่าสังเกตหนึ่งค่าในทุกคู่ของ Replication × A × B"
    )
  ),

  method_topic(
    "split_rcbd",
    "ANOVA: แผนการทดลองแบบ split plot ใน RCBD",
    "aov",
    software = c("stats::aov()"),
    refs = c(
      "Chambers JM, Freeny A",
      "Satterthwaite FE (1946)",
      "Gomez KA, Gomez AA (1984)"
    ),
    p(
      "โมเดล: ",
      code("y ~ Block + A * B + Error(Block:A)"),
      " ประมาณพารามิเตอร์ด้วย ",
      code("aov()"),
      " (Chambers et al., 1992) โดย A เป็นปัจจัยหลักระดับแปลงใหญ่ และ B เป็นปัจจัยรองระดับแปลงย่อย"
    ),
    p(
      "Error(a) คือปฏิสัมพันธ์ระหว่างบล็อกกับปัจจัยหลัก A หรือ Block × A ",
      "ใช้เป็น error term สำหรับทดสอบอิทธิพลของ Block และ A ",
      "ส่วน Error(b) คือความคลาดเคลื่อนที่เหลือในระดับแปลงย่อย ใช้ทดสอบอิทธิพลของ B และปฏิสัมพันธ์ A × B"
    ),
    p(
      "ค่า CV(a) และ CV(b) คำนวณจาก mean square ของ Error(a) และ Error(b) ตามลำดับ ",
      "ข้อมูลต้องมีจำนวนครบและสมดุล กล่าวคือ มีค่าสังเกตหนึ่งค่าในทุกคู่ของ Block × A × B"
    ),
    p(
      "การเปรียบเทียบค่าเฉลี่ยใน split plot ต้องเลือก error term ให้สอดคล้องกับระดับของหน่วยทดลอง ",
      "การเปรียบเทียบค่าเฉลี่ยของ A ใช้ Error(a) ส่วนการเปรียบเทียบค่าเฉลี่ยของ B ใช้ Error(b) ",
      "ค่าความคลาดเคลื่อนมาตรฐานคำนวณจาก sqrt(MS error / n)"
    ),
    p(
      "เมื่อปฏิสัมพันธ์ A × B มีนัยสำคัญ ให้เปรียบเทียบ B ภายในแต่ละระดับของ A โดยใช้ Error(b) ",
      "แต่การเปรียบเทียบ A ภายในแต่ละระดับของ B ต้องใช้ error term ผสม ",
      "MS(error term ผสม) = [MS(a) + (b - 1)MS(b)] / b ",
      "โดยองศาอิสระประมาณด้วยวิธีของ Satterthwaite และปัดลงเป็นจำนวนเต็มก่อนใช้อ้างอิงค่า t (Satterthwaite, 1946; Gomez & Gomez, 1984)"
    )
  ),

  method_topic(
    "repeated",
    "ANOVA: วัดซ้ำตามเวลา (repeated measures)",
    "aov",
    software = c("stats::aov()", "stats::lm()", "stats::mauchly.test()", "stats::pf()"),
    refs = c(
      "Gomez KA, Gomez AA (1984)",
      "Mauchly JW (1940)",
      "Greenhouse SW, Geisser S (1959)",
      "Huynh H, Feldt LS (1976)",
      "Girden ER (1992)"
    ),
    p(
      "แปลง (กระถาง ต้น หรือสัตว์) แต่ละหน่วยได้รับสิ่งทดลอง (T) หนึ่งอย่าง แล้ววัดซ้ำหลายครั้งตามเวลา (Time) ",
      "วิเคราะห์แบบ split plot in time (Gomez & Gomez, 1984) โดยให้สิ่งทดลองเป็นปัจจัยหลักและเวลาเป็นปัจจัยรอง: ",
      code("y ~ Block + T * Time + Error(Block:T)"),
      " สำหรับ RCBD และ ",
      code("y ~ T * Time + Error(Plot:T)"),
      " สำหรับ CRD ประมาณด้วย ",
      code("aov()"),
      " Error(a) (ระหว่างแปลง) ใช้ทดสอบ T ส่วน Error(b) (ภายในแปลง) ใช้ทดสอบ Time และ T × Time"
    ),
    p(
      "เวลาไม่สามารถสุ่มภายในแปลงได้ ค่าที่วัดในเวลาใกล้กันจึงมักคล้ายกันมากกว่าค่าที่วัดห่างกัน ",
      "F test ของ Time และ T × Time ต้องการสมมติฐาน sphericity (ความแปรปรวนของผลต่างระหว่างทุกคู่เวลาเท่ากัน) ",
      "แอปจัดข้อมูลเป็นเมทริกซ์ แปลง × เวลา แปลงเป็น orthonormal contrasts ระหว่างเวลา แล้วหา residual SSCP หลังตัดค่าเฉลี่ยของสิ่งทดลอง ",
      "(บล็อกรวมอยู่ในความคลาดเคลื่อน เช่นเดียวกับ Error(b)) ทดสอบด้วย Mauchly's test (",
      code("mauchly.test()"),
      "; Mauchly, 1940) และคำนวณ epsilon ของ Greenhouse-Geisser (1959) และ Huynh-Feldt (1976) ตรงกับ ",
      code("anova.mlm(test = \"Spherical\")")
    ),
    p(
      "เมื่อ Mauchly's test มีนัยสำคัญ องศาอิสระของ Time และ T × Time จะคูณด้วย epsilon ของ Greenhouse-Geisser ",
      "หรือของ Huynh-Feldt เมื่อ epsilon ของ Greenhouse-Geisser มากกว่า 0.75 (Girden, 1992) ",
      "เมื่อจำนวนแปลงน้อยเกินกว่าจะทดสอบได้ จะใช้ Greenhouse-Geisser เสมอ วัดสองครั้งไม่ต้องปรับ ",
      "ค่า p ที่ปรับแล้วใช้ตัดสินนัยสำคัญและการให้อักษร ส่วนตาราง ANOVA แสดงค่า p ที่ยังไม่ปรับ ",
      "การเปรียบเทียบค่าเฉลี่ยเหมือน split plot (Time ใช้ Error(b) ซึ่งยังอิงสมมติฐาน sphericity) ",
      "ข้อมูลต้องครบ ทุกแปลงต้องมีค่าหนึ่งค่าในทุกเวลา"
    )
  ),

  method_topic(
    "combined",
    "Combined analysis ร่วมหลายสถานที่ / ฤดูกาล",
    "aov",
    software = c(
      "stats::aov()",
      "stats::lm()",
      "stats::bartlett.test()"
    ),
    refs = c("Gomez KA, Gomez AA (1984)", "Bartlett MS (1937)"),
    p(
      "การวิเคราะห์ร่วมใช้กับปัจจัยสิ่งทดลองเดียว (T) ที่ทดลองซ้ำในหลายสภาพแวดล้อม ",
      "(E: สถานที่ ฤดูกาล หรือปี) ตามวิธีของ Gomez & Gomez (1984, บทที่ 8) เช่นเดียวกับโปรแกรม STAR"
    ),
    p(
      "สำหรับข้อมูล RCBD ใช้โมเดล ",
      code("y ~ E + E:Rep + T + T:E"),
      " โดยอิทธิพลของ E ทดสอบด้วย Rep ภายใน E ",
      "ส่วน Rep ภายใน E, T และ T × E ทดสอบด้วย error รวม (pooled error)"
    ),
    p(
      "สำหรับข้อมูล CRD ใช้โมเดล ",
      code("y ~ E + T + T:E"),
      " โดยอิทธิพลทุกอย่างทดสอบด้วย error รวม ",
      "สภาพแวดล้อมถือเป็นอิทธิพลคงที่ (fixed effect) ตามแนวทางของ STAR"
    ),
    p(
      "ก่อนการวิเคราะห์ร่วม โปรแกรมวิเคราะห์ข้อมูลแยกรายสภาพแวดล้อม ",
      "และทดสอบความเท่ากันของความแปรปรวนของ error ระหว่างสภาพแวดล้อมด้วย Bartlett's test ",
      "โดยใช้ mean square และองศาอิสระของ error จากการวิเคราะห์แยกแต่ละสภาพแวดล้อม (Bartlett, 1937) ",
      "คำนวณด้วย ",
      code("bartlett.test()"),
      " กับโมเดล ",
      code("lm()"),
      " ของแต่ละสภาพแวดล้อม"
    ),
    p(
      "หากความแปรปรวนของ error ระหว่างสภาพแวดล้อมแตกต่างกัน การรวม error เป็น pooled error อาจไม่เหมาะสม ",
      "การเปรียบเทียบค่าเฉลี่ยทำเช่นเดียวกับ ANOVA สองปัจจัย ",
      "เมื่อ T × E มีนัยสำคัญ จะเปรียบเทียบ T ภายในแต่ละ E และ E ภายในแต่ละ T โดยใช้ pooled error ",
      "ข้อมูลแบบ RCBD ต้องมีจำนวนครบและสมดุล"
    )
  ),

  method_topic(
    "transform",
    "การแปลงข้อมูล (data transformation)",
    "after",
    software = c("log(), sqrt(), asin() in R"),
    refs = c("Gomez KA, Gomez AA (1984)"),
    p(
      "การแปลงข้อมูลสามารถเลือกใช้ร่วมกับ ANOVA ได้ ",
      "ใช้ log(y + 1) สำหรับข้อมูลการนับที่มีช่วงการกระจายกว้าง ",
      "sqrt(y + 0.5) สำหรับข้อมูลการนับที่มีค่าต่ำ ",
      "และ arcsine ของ sqrt(y / 100) ในหน่วยองศา สำหรับข้อมูลร้อยละในช่วง 0 ถึง 100 ",
      "(Gomez & Gomez, 1984, บทที่ 7)"
    ),
    p(
      "ค่าในตาราง ANOVA ค่าเฉลี่ย SE(m), SED และ LSD รายงานในหน่วยของข้อมูลที่แปลงแล้ว ",
      "ส่วนค่าเฉลี่ยที่แปลงกลับเป็นหน่วยเดิมเพื่อใช้รายงาน คำนวณด้วย exp(m) - 1, m^2 - 0.5 และ 100 sin^2(m) ตามลำดับ"
    )
  ),

  method_topic(
    "pub_table",
    "ตารางค่าเฉลี่ยสำหรับตีพิมพ์",
    "after",
    software = c("stats::qt()"),
    refs = c("Gomez KA, Gomez AA (1984)"),
    p(
      "ใต้ตารางค่าเฉลี่ยของแต่ละปัจจัย แสดงค่า F พร้อมระดับนัยสำคัญ ",
      "(** p < 0.01, * p < 0.05, ns = ไม่มีนัยสำคัญ), ค่าเฉลี่ยรวม และ CV"
    ),
    p(
      "ค่า SE(m) = sqrt(MSE / r), SED = sqrt(2MSE / r) ",
      "และ LSD = t(1 - alpha/2, df) × SED ",
      "โดย MSE และ df ใช้จาก error term ที่ใช้ทดสอบปัจจัยนั้น ",
      "เช่น Error(a) หรือ Error(b) ใน split plot และ r คือจำนวนค่าสังเกตต่อระดับ ",
      "เมื่อจำนวนค่าสังเกตของแต่ละระดับไม่เท่ากัน จะใช้ค่าเฉลี่ยฮาร์มอนิกแทน"
    )
  ),

  method_topic(
    "posthoc",
    "การเปรียบเทียบค่าเฉลี่ย (post hoc tests)",
    "after",
    software = c(
      "agricolae::LSD.test()",
      "agricolae::HSD.test()",
      "agricolae::duncan.test()"
    ),
    refs = c(
      "Fisher RA (1935)",
      "Tukey JW (1949)",
      "Kramer CY (1956)",
      "Duncan DB (1955)"
    ),
    p(
      "การเปรียบเทียบค่าเฉลี่ยหลัง ANOVA มีให้เลือก Fisher's protected LSD (Fisher, 1935), ",
      "Tukey's HSD (Tukey, 1949) และ Duncan's multiple range test หรือ DMRT (Duncan, 1955) ",
      "เมื่อจำนวนข้อมูลในแต่ละกลุ่มไม่เท่ากัน Tukey's HSD ใช้การปรับแบบ Tukey-Kramer (Kramer, 1956)"
    ),
    p(
      "การคำนวณใช้แพ็กเกจ agricolae โดยใช้ mean square และองศาอิสระของ error term ที่เกี่ยวข้อง ",
      "ที่ระดับนัยสำคัญที่กำหนด ค่าเริ่มต้นคือ 0.05"
    ),
    p(
      "ค่าเฉลี่ยที่มีอักษรเหมือนกันแสดงว่าไม่แตกต่างกันอย่างมีนัยสำคัญ ",
      "อักษรจะแสดงเฉพาะเมื่อการทดสอบ F ของอิทธิพลนั้นมีนัยสำคัญ"
    ),
    p(
      "เมื่อมีสองปัจจัยและปฏิสัมพันธ์มีนัยสำคัญ จะเปรียบเทียบอิทธิพลอย่างง่าย ",
      "(simple effects) แทนการเปรียบเทียบค่าเฉลี่ยรวมของแต่ละปัจจัย ",
      "กล่าวคือ เปรียบเทียบระดับของปัจจัยหนึ่งภายในแต่ละระดับของอีกปัจจัย"
    ),
    p(
      "โปรแกรมเลือกวิธีเริ่มต้นโดยอัตโนมัติ คือ ใช้ LSD เมื่อปัจจัยมีไม่เกิน 5 ระดับ ",
      "และใช้ Tukey's HSD เมื่อมีมากกว่า 5 ระดับ ",
      "แนวทางการจัดการกรณีจำนวนซ้ำไม่เท่ากันของแต่ละวิธี ดูรายละเอียดในเอกสารของแพ็กเกจ agricolae"
    )
  ),

  method_topic(
    "assumptions",
    "การตรวจสอบข้อตกลงเบื้องต้น",
    "after",
    software = c(
      "stats::rstudent()",
      "stats::cooks.distance()",
      "stats::shapiro.test()",
      "lmtest::bptest()"
    ),
    refs = c(
      "Belsley DA",
      "Cook RD (1977)",
      "Shapiro SS, Wilk MB (1965)",
      "Brown MB, Forsythe AB (1974)",
      "Breusch TS",
      "Koenker R (1981)"
    ),
    p(
      "ประมาณโมเดลเชิงเส้นเดิมอีกครั้งเพื่อตรวจสอบค่า residuals ",
      "ขณะนี้ยังไม่รองรับการตรวจสอบข้อตกลงเบื้องต้นสำหรับแผนการทดลองแบบ split plot"
    ),
    p(
      "การตรวจค่าผิดปกติใช้ externally studentized residuals ",
      "โดยค่าที่มี |r| > 3 ถือเป็นค่าผิดปกติ และค่าที่มี 2 < |r| <= 3 ถือว่าน่าสงสัย ",
      "(Belsley et al., 1980) พร้อมแสดงค่า Cook's distance (Cook, 1977) ของแถวที่ถูกระบุ"
    ),
    p(
      "การทดสอบการแจกแจงปกติของค่าส่วนเหลือ ใช้ Shapiro-Wilk test (Shapiro & Wilk, 1965)"
    ),
    p(
      "การตรวจสอบความเท่ากันของความแปรปรวนใน ANOVA ใช้ Levene's test แบบใช้มัธยฐาน ",
      "หรือ Brown-Forsythe test (Brown & Forsythe, 1974) เปรียบเทียบระหว่างกลุ่มสิ่งทดลอง"
    ),
    p(
      "การตรวจสอบความแปรปรวนคงที่ในการถดถอย ใช้ Breusch-Pagan test (Breusch & Pagan, 1979) ",
      "ในรูปแบบ studentized (Koenker, 1981) เทียบกับค่าที่โมเดลประมาณได้"
    ),
    p(
      "ผลการทดสอบเหล่านี้ใช้เป็นข้อแนะนำ ไม่ใช่ข้อพิสูจน์ ",
      "เมื่อจำนวนข้อมูลน้อย อำนาจการทดสอบอาจต่ำ ",
      "แต่เมื่อจำนวนข้อมูลมาก การทดสอบอาจชี้ความเบี่ยงเบนเล็กน้อยที่ไม่มีความสำคัญในทางปฏิบัติ"
    )
  ),

  method_topic(
    "advice",
    "คำแนะนำการเลือกวิธีวิเคราะห์",
    "alt",
    software = character(),
    refs = character(),
    p(
      "ผลการตรวจสอบข้อตกลงเบื้องต้นแต่ละข้อ ได้แก่ การแจกแจงปกติ ความแปรปรวน และค่าผิดปกติ ",
      "แสดงเป็นไฟ 3 สี ทั้งในกล่องคำแนะนำและแท็บ Outliers & assumptions"
    ),
    p(
      strong("เขียว"),
      " หมายถึงผ่านข้อตกลงเบื้องต้น สามารถใช้วิธีพาราเมตริกได้ ",
      strong("เหลือง"),
      " หมายถึงมีความเบี่ยงเบนเล็กน้อยแต่วิธีพาราเมตริกยังคงทนทานพอ ",
      "เช่น ค่าส่วนเหลือไม่เป็นไปตามการแจกแจงปกติ แต่มีข้อมูลตั้งแต่ 30 ค่าต่อกลุ่มและรูปร่างการกระจายไม่เบ้รุนแรง ",
      "หรือความแปรปรวนระหว่างกลุ่มต่างกันไม่เกิน 4 เท่าเมื่อจำนวนข้อมูลเท่ากัน ",
      "หรือมีค่าน่าสงสัย ได้แก่ 2 < |studentized residual| <= 3 หรือค่าอยู่นอก 1.5 × IQR ",
      "หรือมีข้อมูลน้อยกว่า 15 ค่าต่อกลุ่ม ซึ่งทำให้อำนาจการทดสอบต่ำ"
    ),
    p(
      strong("แดง"),
      " หมายถึงการละเมิดข้อตกลงเบื้องต้นที่ชัดเจน ",
      "เช่น ค่าส่วนเหลือไม่เป็นไปตามการแจกแจงปกติและไม่เข้าเกณฑ์ความทนทาน ",
      "ความแปรปรวนระหว่างกลุ่มแตกต่างกันมาก หรือมีค่าผิดปกติ ",
      "|studentized residual| > 3 หรือค่าอยู่นอก 3 × IQR ",
      "ในกรณีนี้แนะนำให้พิจารณาเปลี่ยนวิธีการวิเคราะห์"
    ),
    p(
      "เมื่อแสดงไฟแดง แอปจะเสนอทางเลือกตามวัตถุประสงค์ของการรายงาน ",
      "หากต้องการเปรียบเทียบอันดับหรือมัธยฐาน แนะนำ Kruskal-Wallis, Friedman หรือ Wilcoxon ",
      "หากต้องการเปรียบเทียบค่าเฉลี่ยโดยไม่อาศัยสมมติการแจกแจงปกติ ",
      "แนะนำค่าเฉลี่ยตัดปลาย 20% พร้อมช่วงความเชื่อมั่นแบบ bootstrap ",
      "สำหรับ CRD ปัจจัยเดียวและการเปรียบเทียบสองกลุ่ม ",
      "หากพบค่าผิดปกติ ควรตรวจสอบก่อนว่าเกิดจากการบันทึกข้อมูลหรือการวัดผิดพลาดหรือไม่"
    ),
    p(
      "หลังการวิเคราะห์ แอปตรวจสอบข้อมูลและแนะนำวิธีที่เหมาะสม พร้อมปุ่มสำหรับเปลี่ยนวิธี ",
      "คำแนะนำมีลักษณะเป็นข้อเสนอ ผู้วิเคราะห์เป็นผู้ตัดสินใจขั้นสุดท้าย"
    ),
    p(
      "ลำดับการตรวจสอบสำหรับ CRD และ RCBD ปัจจัยเดียว และการเปรียบเทียบสองกลุ่ม มีดังนี้ ",
      "(1) ตรวจการแจกแจงปกติของค่าส่วนเหลือของโมเดล ",
      "สำหรับการเปรียบเทียบสองกลุ่ม ให้ตรวจค่าส่วนเหลือของแต่ละกลุ่ม ",
      "หรือค่าส่วนเหลือของผลต่างในกรณีข้อมูลจับคู่ ไม่ใช่การแจกแจงของข้อมูลรวมทุกกลุ่ม"
    ),
    p(
      "หากการทดสอบความปกติไม่ผ่าน แต่มีข้อมูลตั้งแต่ 30 ค่าต่อกลุ่ม ",
      "และ |skewness| <= 1 และ |excess kurtosis| <= 2 ",
      "ถือว่าวิธีพาราเมตริกยังคงทนทานได้ โดยอาศัยหลัก central limit theorem ",
      "เกณฑ์นี้เป็นแนวทางตามธรรมเนียม ไม่ใช่เกณฑ์ตายตัว ",
      "หากมีข้อมูลน้อยกว่า 15 ค่าต่อกลุ่ม แอปจะเตือนว่า Shapiro-Wilk test มีอำนาจการทดสอบต่ำ"
    ),
    p(
      "(2) หากค่าส่วนเหลือไม่เป็นไปตามการแจกแจงปกติ แอปจะตรวจก่อนว่าเกิดจากค่าผิดปกติหรือไม่ ",
      "สำหรับ ANOVA จะตัดค่าที่มี |studentized residual| > 3 แล้วทดสอบซ้ำ ",
      "สำหรับการเปรียบเทียบสองกลุ่ม จะพิจารณาค่าที่อยู่นอก 1.5 × IQR ",
      "และแนะนำให้ตรวจสอบค่าเหล่านั้นก่อนตัดสินใจ ",
      "หากข้อมูลเป็นบวกและเบ้ขวา แนะนำการแปลงข้อมูลก่อน ",
      "จากนั้นจึงพิจารณาการทดสอบแบบอันดับ"
    ),
    p(
      "(3) ตรวจสอบความแปรปรวน ",
      "สำหรับ CRD ปัจจัยเดียวและการเปรียบเทียบสองกลุ่ม ",
      "แนะนำ Welch's test เมื่อความแปรปรวนไม่เท่ากัน ",
      "โดยพิจารณาจาก Brown-Forsythe test ที่มีนัยสำคัญ ",
      "หรือเมื่อจำนวนข้อมูลของแต่ละกลุ่มไม่เท่ากันและความแปรปรวนต่างกันตั้งแต่ 2 เท่าขึ้นไป ",
      "สำหรับ RCBD ซึ่งไม่มี Welch's test ที่คำนึงถึงบล็อก ",
      "หากความแปรปรวนไม่เท่ากัน แนะนำการแปลงข้อมูลหรือ Friedman test"
    ),
    p(
      "แอปแสดงผลเฉพาะวิธีที่ผู้ใช้เลือก ไม่แสดงผลของวิธีอื่นคู่กัน ",
      "เพื่อป้องกันการเลือกวิธีโดยอิงจากค่า p ที่ได้ ",
      "การเลือกวิธีจากผลการทดสอบข้อตกลงเบื้องต้นบนข้อมูลชุดเดียวกัน อาจทำให้ค่า p ของการทดสอบหลักคลาดเคลื่อนได้ ",
      "จึงควรตัดสินใจจากลักษณะข้อมูล การออกแบบการทดลอง และวัตถุประสงค์ของการวิเคราะห์เป็นหลัก"
    )
  ),

  method_topic(
    "welch_default",
    "ค่าเริ่มต้น: ANOVA แบบดั้งเดิม แล้วแนะนำ Welch's test เมื่อจำเป็น",
    "alt",
    software = c("stats::t.test()", "stats::oneway.test()"),
    refs = c(
      "Delacre M, Lakens D",
      "Delacre M, Leys C",
      "Zimmerman DW (2004)"
    ),
    p(
      "แอปใช้ ANOVA แบบดั้งเดิมเป็นค่าเริ่มต้น ",
      "และใช้ pooled t-test สำหรับการเปรียบเทียบสองกลุ่ม ",
      "เนื่องจากเป็นวิธีที่ผู้วิเคราะห์ส่วนใหญ่คุ้นเคยและเป็นรูปแบบที่นิยมใช้รายงาน"
    ),
    p(
      "หลังการวิเคราะห์ แอปจะตรวจสอบข้อตกลงเบื้องต้นโดยอัตโนมัติ ",
      "หากความแปรปรวนระหว่างกลุ่มไม่เท่ากัน จะแนะนำ Welch's test พร้อมปุ่มสำหรับเปลี่ยนวิธี ",
      "แต่จะไม่บังคับให้เปลี่ยน"
    ),
    p(
      "เหตุผลที่แนะนำ Welch's test มีดังนี้ ",
      "(1) เมื่อความแปรปรวนเท่ากันจริง ผลของ Welch's test แทบไม่ต่างจากวิธีดั้งเดิม ",
      "แต่เมื่อความแปรปรวนไม่เท่ากัน โดยเฉพาะเมื่อจำนวนข้อมูลของแต่ละกลุ่มไม่เท่ากัน ",
      "วิธีดั้งเดิมอาจให้ค่า p คลาดเคลื่อนได้มาก (Delacre et al., 2017; Delacre et al., 2019)"
    ),
    p(
      "(2) R กำหนด ",
      code("var.equal = FALSE"),
      " เป็นค่าเริ่มต้นของ ",
      code("t.test()"),
      " และ ",
      code("oneway.test()")
    ),
    p(
      "(3) หากเลือกวิธีทดสอบค่าเฉลี่ยโดยอิงจากผลการทดสอบความแปรปรวนก่อน ",
      "อัตราการเกิด Type I error ของการทดสอบค่าเฉลี่ยที่ตามมาอาจคลาดเคลื่อน (Zimmerman, 2004) ",
      "แอปจึงใช้ผล Levene's test เป็นเพียงข้อแนะนำสำหรับการตัดสินใจ ",
      "และไม่เปลี่ยนวิธีการวิเคราะห์โดยอัตโนมัติ"
    ),
    p(
      "ตัวเลือกที่ไม่รองรับแผนการทดลองที่เลือกจะแสดงเป็นสีเทาและไม่สามารถเลือกได้ ",
      "Welch's ANOVA และ trimmed means มีเฉพาะ CRD ปัจจัยเดียว เนื่องจากไม่มีรูปแบบที่คำนึงถึงบล็อก ",
      "การทดสอบแบบอันดับรองรับ CRD และ RCBD ปัจจัยเดียว ",
      "ส่วนการทดลองสองปัจจัย split plot และ combined analysis ใช้ ANOVA แบบดั้งเดิมเท่านั้น"
    ),
    p(
      "Welch's ANOVA ไม่มีตารางค่าเฉลี่ยสำหรับการตีพิมพ์ที่รายงาน SE(m), SED, LSD และ CV ",
      "จากความแปรปรวนร่วม เนื่องจากวิธีนี้ไม่สมมติว่าความแปรปรวนของทุกกลุ่มเท่ากัน ",
      "หากต้องการตารางในรูปแบบดังกล่าว ให้เลือก ANOVA แบบดั้งเดิม"
    )
  ),

  method_topic(
    "trimmed",
    "ค่าเฉลี่ยตัดปลาย (trimmed means) และ bootstrap",
    "alt",
    software = c(
      "code of this app, following Wilcox (2017)",
      "stats::p.adjust() (Holm)"
    ),
    refs = c(
      "Wilcox RR (2017)",
      "Yuen KK (1974)",
      "Efron B, Tibshirani RJ (1993)",
      "Holm S (1979)"
    ),
    p(
      "ตัดค่าต่ำสุดและสูงสุดของแต่ละกลุ่มออกข้างละ 20% แล้วเปรียบเทียบค่าเฉลี่ยของส่วนที่เหลือ ",
      "ข้อมูลที่เบ้หรือมีค่าสุดโต่งเพียงไม่กี่ค่าจึงไม่ครอบงำผลการวิเคราะห์ ",
      "วิธีนี้ใช้ความแปรปรวนแบบ Winsorized และไม่สมมติว่าความแปรปรวนเท่ากัน (Wilcox, 2017)"
    ),
    p(
      "สองกลุ่มอิสระใช้ Yuen's test (Yuen, 1974) ",
      "ข้อมูลจับคู่ทดสอบค่าเฉลี่ยตัดปลายของผลต่าง ",
      "และกลุ่มเดียวทดสอบค่าเฉลี่ยตัดปลายเทียบกับค่าที่กำหนด ",
      "สำหรับ CRD ปัจจัยเดียวใช้ heteroscedastic one-way ANOVA บนค่าเฉลี่ยตัดปลาย (t1way ของ Wilcox) ",
      "และเปรียบเทียบรายคู่ด้วย Yuen's test พร้อมปรับค่า p แบบ Holm เพื่อกำหนดอักษร"
    ),
    p(
      "ช่วงความเชื่อมั่นเป็นแบบ percentile bootstrap (Efron & Tibshirani, 1993) จากการสุ่มซ้ำ 2,000 ครั้งด้วย seed คงที่ ",
      "ข้อมูลชุดเดิมจึงได้ช่วงความเชื่อมั่นเดิมทุกครั้ง ",
      "การ Winsorize ใช้ order statistics ตามนิยามของ Wilcox (เช่นเดียวกับแพ็กเกจ WRS2) ",
      "ซึ่งอาจต่างเล็กน้อยจากบางโปรแกรมที่ใช้ quantile แบบประมาณค่า"
    ),
    p(
      "เมื่อไม่ตัดปลาย (0%) วิธีเหล่านี้คือ Welch's t-test และ Welch's ANOVA ",
      "เมื่อมีข้อมูลน้อยกว่า 10 ค่าต่อกลุ่ม การตัดปลาย 20% แทบไม่ตัดค่าใดออก และช่วง bootstrap จะหยาบ แอปจะเตือนในกรณีนี้ ",
      "RCBD ไม่มีวิธีนี้"
    )
  ),

  method_topic(
    "welch_anova",
    "Welch's ANOVA",
    "alt",
    software = c(
      "stats::oneway.test()",
      "Games-Howell test: code of this app with stats::ptukey()"
    ),
    refs = c("Welch BL (1951)", "Games PA, Howell JF (1976)"),
    p(
      "ใช้ ",
      code("oneway.test(var.equal = FALSE)"),
      " (Welch, 1951) สำหรับ CRD ปัจจัยเดียว โดยไม่สมมติว่าความแปรปรวนเท่ากัน"
    ),
    p(
      "เปรียบเทียบค่าเฉลี่ยรายคู่ด้วย Games-Howell test (Games & Howell, 1976) ",
      "ซึ่งใช้ความแปรปรวนของแต่ละคู่ องศาอิสระแบบ Welch และการแจกแจง studentized range ",
      "อักษรแสดงเฉพาะเมื่อ Welch's test มีนัยสำคัญ ",
      "และใช้ค่าที่วิเคราะห์จริง (ค่าที่แปลงแล้ว หากเลือกการแปลงข้อมูล)"
    )
  ),

  method_topic(
    "nonparametric",
    "การทดสอบแบบไม่อิงพารามิเตอร์ (nonparametric)",
    "alt",
    software = c(
      "agricolae::kruskal()",
      "agricolae::friedman()",
      "Dunn's test: code of this app",
      "stats::wilcox.test()"
    ),
    refs = c(
      "Kruskal WH",
      "Friedman M (1937)",
      "Conover WJ (1999)",
      "Dunn OJ (1964)",
      "Holm S (1979)",
      "Wilcoxon F (1945)",
      "Mann HB",
      "Hodges JL"
    ),
    p(
      "การทดสอบแบบไม่อิงพารามิเตอร์เปรียบเทียบอันดับ (rank) ของข้อมูล แทนการเปรียบเทียบค่าเฉลี่ย ",
      "จึงไม่ต้องอาศัยข้อตกลงเบื้องต้นเรื่องการแจกแจงปกติและความแปรปรวนเท่ากัน ",
      "และไม่ถือเป็นการวิเคราะห์ความแปรปรวน"
    ),
    p(
      "ผู้ใช้เลือกประเภทผลที่ต้องการรายงานได้ ",
      "ในหน้า Compare treatments ให้เลือกที่ Analysis ",
      "และในหน้า Two groups ให้เลือกที่ Test ",
      "เมื่อเลือกการทดสอบแบบอันดับ ผลหลัก อักษรเปรียบเทียบ การรายงานตามรูปแบบ APA ",
      "ไฟล์ Excel/Word และ R code จะอ้างอิงจากการทดสอบนี้"
    ),
    p(
      "เมื่อเลือกวิธีพาราเมตริก แอปจะไม่แสดงผลของการทดสอบแบบอันดับ ",
      "แต่จะแนะนำเมื่อข้อมูลไม่เป็นไปตามข้อตกลงเบื้องต้น ",
      "ควรเลือกวิธีการวิเคราะห์ก่อนพิจารณาผล ไม่ใช่เลือกวิธีที่ให้ค่า p ต่ำกว่า"
    ),
    p(
      "สำหรับการเปรียบเทียบสิ่งทดลองหนึ่งปัจจัย ",
      "CRD ใช้ Kruskal-Wallis test (Kruskal & Wallis, 1952) ",
      "และ RCBD ใช้ Friedman test (Friedman, 1937) โดยจัดอันดับภายในแต่ละบล็อก"
    ),
    p(
      "การคำนวณใช้แพ็กเกจ agricolae ",
      "ซึ่งใช้การเปรียบเทียบอันดับแบบ Fisher's LSD (Conover, 1999) เป็นค่าเริ่มต้น ",
      "หลัง Kruskal-Wallis test สามารถเลือก Dunn's test (Dunn, 1964) ",
      "พร้อมการปรับค่า p แบบ Holm (Holm, 1979) หรือ Bonferroni ได้"
    ),
    p(
      "อักษรเปรียบเทียบจะแสดงเฉพาะเมื่อการทดสอบรวมมีนัยสำคัญ ",
      "หากรูปร่างการกระจายของแต่ละกลุ่มแตกต่างกันมาก ",
      "การทดสอบจะสะท้อนความแตกต่างของอันดับเฉลี่ย ",
      "หรือแนวโน้มว่ากลุ่มใดมีค่าสูงกว่า มากกว่าความแตกต่างของมัธยฐานโดยตรง"
    ),
    p(
      "การวิเคราะห์ใช้ค่าข้อมูลเดิม ไม่มีการแปลงข้อมูล เนื่องจากการแปลงแบบ monotonic ไม่เปลี่ยนอันดับของข้อมูล ",
      "ค่ามัธยฐานที่รายงานจึงอยู่ในหน่วยเดิมของข้อมูล"
    ),
    p(
      "ไม่มีการทดสอบแบบอันดับสำหรับการทดลองสองปัจจัย split plot และ combined analysis ",
      "Friedman test ต้องมีข้อมูลครบหนึ่งค่าสำหรับทุกสิ่งทดลองในทุกบล็อก ",
      "หากข้อมูลไม่ครบ แอปจะแจ้งสาเหตุ"
    ),
    p(
      "สำหรับการเปรียบเทียบสองกลุ่ม ",
      "สองกลุ่มอิสระใช้ Wilcoxon rank-sum test หรือ Mann-Whitney test ",
      "(Wilcoxon, 1945; Mann & Whitney, 1947) ",
      "ส่วนข้อมูลจับคู่และกลุ่มเดียวใช้ Wilcoxon signed-rank test ผ่าน ",
      code("wilcox.test()")
    ),
    p(
      "ค่าประมาณและช่วงความเชื่อมั่นเป็นของ Hodges-Lehmann (Hodges & Lehmann, 1963) ",
      "ค่า p จะเป็นค่าแม่นตรง (exact) เมื่อมีข้อมูลน้อยกว่า 50 ค่าและไม่มีค่าซ้ำ ",
      "หากมีค่าซ้ำ ผลต่างเป็นศูนย์ในกรณีข้อมูลจับคู่ หรือมีข้อมูลตั้งแต่ 50 ค่าขึ้นไป ",
      "จะใช้การประมาณด้วยการแจกแจงปกติพร้อม continuity correction ",
      "และอาจไม่สามารถคำนวณช่วงความเชื่อมั่นได้"
    )
  ),

  method_topic(
    "correlation",
    "สหสัมพันธ์ (correlation)",
    "corr",
    software = c("stats::cor.test()", "stats::p.adjust()"),
    refs = c(
      "Pearson K (1895)",
      "Spearman C (1904)",
      "Kendall MG (1938)",
      "Hollander M",
      "Best DJ",
      "Fisher RA (1921)",
      "Holm S (1979)",
      "Benjamini Y"
    ),
    p(
      "วิเคราะห์สหสัมพันธ์แบบ Pearson, Spearman หรือ Kendall สำหรับตัวแปรแต่ละคู่ด้วย ",
      code("cor.test()"),
      " (Pearson, 1895; Spearman, 1904; Kendall, 1938; Hollander & Wolfe, 1973; ",
      "ค่า p ของ Spearman's rho: Best & Roberts, 1975)"
    ),
    p(
      "แต่ละคู่ตัวแปรใช้เฉพาะแถวที่มีค่าของตัวแปรทั้งสองตัว และแสดงจำนวนข้อมูล N ของแต่ละคู่"
    ),
    p(
      "ช่วงความเชื่อมั่นของ Pearson's r คำนวณโดยใช้ Fisher's z transformation (Fisher, 1921) ",
      "การปรับค่า p เมื่อทดสอบหลายคู่ ซึ่งเลือกได้ ใช้ ",
      code("p.adjust()"),
      " โดยมี Holm (1979), Bonferroni และ Benjamini-Hochberg (1995)"
    ),
    p(
      "คำว่า อ่อน ปานกลาง และสูง เป็นเกณฑ์ตีความค่าสหสัมพันธ์อย่างคร่าว ๆ ของแอปนี้ ",
      "โดย |r| < 0.1 ถือว่าแทบไม่มีความสัมพันธ์, |r| < 0.3 อ่อน, |r| < 0.5 ปานกลาง, ",
      "|r| < 0.7 สูง และ |r| >= 0.7 สูงมาก ",
      "เกณฑ์นี้ไม่ใช่ผลจากการทดสอบทางสถิติ"
    )
  ),

  method_topic(
    "regression",
    "การถดถอย (regression)",
    "corr",
    software = c(
      "stats::lm()",
      "stats::AIC()",
      "stats::predict()",
      "mctest::imcdiag()"
    ),
    refs = c(
      "Wilkinson GN",
      "Chambers JM (1992)",
      "Akaike H (1974)",
      "Belsley DA"
    ),
    p(
      "ใช้วิธีกำลังสองน้อยที่สุด (ordinary least squares) ผ่าน ",
      code("lm()"),
      " (Wilkinson & Rogers, 1973; Chambers, 1992) ",
      "สำหรับโมเดลเส้นตรง หรือพหุนามของตัวแปรอิสระหนึ่งตัวที่มีกำลังสองหรือกำลังสาม ",
      "โดยใช้ x, x^2 และ x^3 เป็นพจน์ในโมเดล หรือการถดถอยพหุคูณที่มีตัวแปรอิสระหลายตัว"
    ),
    p(
      "รายงานการทดสอบ F ของโมเดล R-squared, adjusted R-squared, ",
      "root mean square error, CV และ AIC (Akaike, 1974) ",
      "พร้อมการทดสอบ t และช่วงความเชื่อมั่นของสัมประสิทธิ์ถดถอยแต่ละตัว"
    ),
    p(
      "สัมประสิทธิ์มาตรฐาน แสดงเฉพาะโมเดลเส้นตรง คำนวณจาก b × SD(x) / SD(y) ",
      "ค่า variance inflation factor หรือ VIF ของตัวแปรอิสระ ",
      "คือ 1 / (1 - R²) ของการถดถอยตัวแปรนั้นด้วยตัวแปรอิสระที่เหลือ คำนวณด้วย ",
      code("imcdiag()"),
      " ของแพ็กเกจ mctest ",
      "ค่า VIF ที่มากกว่า 10 จะถูกระบุว่าอาจมีปัญหา multicollinearity (Belsley et al., 1980)"
    ),
    p(
      "สำหรับโมเดลพหุนามกำลังสอง จุดวิกฤต (stationary point) คำนวณจาก -b1 / (2b2) ",
      "หากจุดดังกล่าวอยู่นอกช่วงของข้อมูล จะระบุว่าเป็นการประมาณนอกช่วง (extrapolation)"
    ),
    p(
      "ค่าทำนายสำหรับโมเดลที่มีตัวแปรอิสระหนึ่งตัว คือค่าเฉลี่ยที่โมเดลประมาณได้ ",
      "พร้อมช่วงความเชื่อมั่น 95% ",
      "ค่าทำนายที่อยู่นอกช่วงของข้อมูลจะถูกระบุว่าเป็นการประมาณนอกช่วง"
    )
  ),

  method_topic(
    "data",
    "การจัดการข้อมูล",
    "data",
    software = c("readxl::read_excel()", "writexl::write_xlsx()"),
    refs = character(),
    p(
      "ไฟล์ที่อัปโหลดจะถูกอ่านภายในเบราว์เซอร์ ",
      "ตัวเลขที่มีตัวคั่นหลักพันหรือระบุหน่วย เช่น 1,250.5 หรือ 12 kg ",
      "จะถูกแปลงเป็นค่าตัวเลข ",
      "ค่าที่เขียนเป็น n/a ถือเป็นค่าสูญหาย ",
      "กล่อง Data checks ในแต่ละหน้าจอจะแสดงรายการค่าที่ถูกแปลงหรือตัดออก"
    )
  ),

  method_topic(
    "missing",
    "การจัดการค่าที่หายไป (missing data)",
    "data",
    software = c("stats::lm()", "stats::predict()"),
    refs = c("Yates F (1933)", "Gomez KA, Gomez AA (1984)"),
    p(
      "ค่าที่ถือว่าหายไป ได้แก่ ช่องว่าง และค่าที่เขียนเป็น NA, n/a, NaN, null, none, missing, -, -- หรือ . ",
      "รวมถึงข้อความที่อยู่ในคอลัมน์ตัวเลข แอปไม่แก้ไขหรือเติมค่าใด ๆ ให้เอง ยกเว้นกรณี RCBD ด้านล่าง ",
      "และกล่อง Data checks ของแต่ละหน้าจอจะแสดงจำนวนแถวที่ถูกตัดออกหรือค่าที่ถูกประมาณ"
    ),
    p("การจัดการค่าที่หายไปของแต่ละการวิเคราะห์:"),
    tags$ul(
      tags$li(strong("t-test และ Wilcoxon: "), "สองกลุ่มอิสระและกลุ่มเดียวตัดเฉพาะแถวที่ไม่มีค่า (แถวที่ไม่ระบุกลุ่มก็ตัดออก) ส่วนแบบจับคู่ใช้เฉพาะคู่ที่มีค่าครบทั้งสองครั้ง"),
      tags$li(strong("ANOVA แบบ CRD, Welch's ANOVA, ค่าเฉลี่ยตัดปลาย และ Kruskal-Wallis: "), "ตัดแถวที่ไม่มีค่าออก แล้ววิเคราะห์ข้อมูลที่เหลือ ซึ่งจำนวนซ้ำของแต่ละสิ่งทดลองอาจไม่เท่ากัน"),
      tags$li(strong("ANOVA แบบ RCBD: "), "ประมาณค่าที่หายไปด้วยวิธีกำลังสองน้อยที่สุดของ Yates (1933) ได้ไม่เกิน 10% ของจำนวนแปลงทั้งหมด (ดูรายละเอียดด้านล่าง)"),
      tags$li(strong("Split plot และ combined analysis: "), "ต้องมีข้อมูลครบทุกแปลง แอปไม่ประมาณค่าให้ หากมีค่าหายไป แอปจะแจ้งให้ตัดออกหรือเติมค่าก่อน"),
      tags$li(strong("Friedman test: "), "ต้องมีค่าครบหนึ่งค่าสำหรับทุกสิ่งทดลองในทุกบล็อก"),
      tags$li(strong("สหสัมพันธ์: "), "แต่ละคู่ตัวแปรใช้เฉพาะแถวที่มีค่าทั้งสองตัว (pairwise) จำนวนข้อมูล N ของแต่ละคู่จึงอาจต่างกัน"),
      tags$li(strong("การถดถอย: "), "ตัดแถวที่มีค่าหายไปในตัวแปรใดตัวแปรหนึ่งที่เลือกไว้ (listwise) และแจ้งหมายเลขแถวที่ถูกตัด")
    ),
    p(
      strong("การประมาณค่าที่หายไปใน RCBD: "),
      "แปลงที่ไม่มีค่า หรือคู่ block × treatment ที่ไม่มีแถวในไฟล์ ถือเป็นแปลงที่หายไป ",
      "ค่าประมาณคือค่าที่ทำให้ผลบวกกำลังสองของ error ต่ำที่สุด ซึ่งเท่ากับค่าทำนายจากโมเดล ",
      code("y ~ Block + Treatment"),
      " ที่ fit กับข้อมูลที่มีอยู่ด้วย ",
      code("lm()"),
      " และ ",
      code("predict()"),
      " จากนั้นวิเคราะห์ข้อมูลที่เติมแล้วด้วย ANOVA และลดองศาอิสระของ error และของผลรวมลงเท่ากับจำนวนค่าที่ประมาณ (Gomez & Gomez, 1984) ",
      "ค่าเฉลี่ยในตารางคำนวณจากข้อมูลที่มีอยู่จริงเท่านั้น ไม่รวมค่าที่ประมาณ"
    ),
    p(
      strong("เงื่อนไขเดียวกับโปรแกรม STAR: "),
      "ประมาณค่าได้ไม่เกิน 10% ของจำนวนแปลงทั้งหมด (เช่น RCBD 6 สิ่งทดลอง × 4 บล็อก = 24 แปลง ประมาณได้ไม่เกิน 2 แปลง) ",
      "และแต่ละคู่ block × treatment ต้องมีไม่เกินหนึ่งแถว หากเกินเงื่อนไข แอปจะไม่วิเคราะห์และแจ้งเหตุผล ",
      "เพราะเมื่อค่าหายไปมาก ค่าประมาณจะมีผลต่อการทดสอบมากขึ้น และความแม่นยำของการเปรียบเทียบจะลดลง"
    ),
    p(
      strong("ข้อควรระวัง: "),
      "วิธีนี้เหมาะเมื่อค่าหายไปโดยไม่เกี่ยวกับสิ่งทดลอง เช่น แปลงเสียหายจากเหตุบังเอิญหรือบันทึกข้อมูลตกหล่น ",
      "หากค่าหายไปเพราะสิ่งทดลอง เช่น ต้นพืชตายเพราะสิ่งทดลองนั้น ไม่ควรประมาณค่า เพราะจะบดบังผลของสิ่งทดลอง ",
      "ค่า F จากข้อมูลที่ประมาณค่าแล้วอาจสูงกว่าค่าที่ได้จากการวิเคราะห์ข้อมูลไม่สมดุลโดยตรงเล็กน้อย (Yates, 1933) ",
      "ควรรายงานจำนวนแปลงที่ประมาณค่าไว้ในวิธีการด้วย"
    )
  )
)

# references for the methods above (software references are generated from the packages in app.R)
method_refs <- c(
  "Akaike H (1974). A new look at the statistical model identification. IEEE Transactions on Automatic Control, 19(6), 716-723. https://doi.org/10.1109/TAC.1974.1100705",
  "Bartlett MS (1937). Properties of sufficiency and statistical tests. Proceedings of the Royal Society of London, Series A, 160(901), 268-282. https://doi.org/10.1098/rspa.1937.0109",
  "Belsley DA, Kuh E, Welsch RE (1980). Regression Diagnostics: Identifying Influential Data and Sources of Collinearity. Wiley, New York.",
  "Benjamini Y, Hochberg Y (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society B, 57(1), 289-300. https://doi.org/10.1111/j.2517-6161.1995.tb02031.x",
  "Best DJ, Roberts DE (1975). Algorithm AS 89: the upper tail probabilities of Spearman's rho. Applied Statistics, 24(3), 377-379.",
  "Breusch TS, Pagan AR (1979). A simple test for heteroscedasticity and random coefficient variation. Econometrica, 47(5), 1287-1294. https://doi.org/10.2307/1911963",
  "Brown MB, Forsythe AB (1974). Robust tests for the equality of variances. Journal of the American Statistical Association, 69(346), 364-367. https://doi.org/10.1080/01621459.1974.10482955",
  "Chambers JM (1992). Linear models. Chapter 4 in Chambers JM, Hastie TJ (eds), Statistical Models in S. Wadsworth & Brooks/Cole, Pacific Grove.",
  "Chambers JM, Freeny A, Heiberger RM (1992). Analysis of variance; designed experiments. Chapter 5 in Chambers JM, Hastie TJ (eds), Statistical Models in S. Wadsworth & Brooks/Cole, Pacific Grove.",
  "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd edition. Lawrence Erlbaum, Hillsdale, NJ.",
  "Conover WJ (1999). Practical Nonparametric Statistics, 3rd edition. John Wiley & Sons, New York.",
  "Cook RD (1977). Detection of influential observation in linear regression. Technometrics, 19(1), 15-18. https://doi.org/10.1080/00401706.1977.10489493",
  "Delacre M, Lakens D, Leys C (2017). Why psychologists should by default use Welch's t-test instead of Student's t-test. International Review of Social Psychology, 30(1), 92-101. https://doi.org/10.5334/irsp.82",
  "Delacre M, Leys C, Mora YL, Lakens D (2019). Taking parametric assumptions seriously: arguments for the use of Welch's F-test instead of the classical F-test in one-way ANOVA. International Review of Social Psychology, 32(1), 13. https://doi.org/10.5334/irsp.198",
  "Duncan DB (1955). Multiple range and multiple F tests. Biometrics, 11(1), 1-42. https://doi.org/10.2307/3001478",
  "Dunn OJ (1964). Multiple comparisons using rank sums. Technometrics, 6(3), 241-252. https://doi.org/10.1080/00401706.1964.10490181",
  "Efron B, Tibshirani RJ (1993). An Introduction to the Bootstrap. Chapman & Hall, New York.",
  "Fisher RA (1921). On the 'probable error' of a coefficient of correlation deduced from a small sample. Metron, 1(4), 3-32.",
  "Fisher RA (1935). The Design of Experiments. Oliver and Boyd, Edinburgh.",
  "Friedman M (1937). The use of ranks to avoid the assumption of normality implicit in the analysis of variance. Journal of the American Statistical Association, 32(200), 675-701. https://doi.org/10.1080/01621459.1937.10503522",
  "Games PA, Howell JF (1976). Pairwise multiple comparison procedures with unequal n's and/or variances: a Monte Carlo study. Journal of Educational Statistics, 1(2), 113-125. https://doi.org/10.3102/10769986001002113",
  "Girden ER (1992). ANOVA: Repeated Measures. Sage University Paper Series on Quantitative Applications in the Social Sciences, 07-084. Sage, Newbury Park.",
  "Gomez KA, Gomez AA (1984). Statistical Procedures for Agricultural Research, 2nd edition. Wiley, New York.",
  "Greenhouse SW, Geisser S (1959). On methods in the analysis of profile data. Psychometrika, 24(2), 95-112. https://doi.org/10.1007/BF02289823",
  "Hodges JL, Lehmann EL (1963). Estimates of location based on rank tests. The Annals of Mathematical Statistics, 34(2), 598-611. https://doi.org/10.1214/aoms/1177704172",
  "Hollander M, Wolfe DA (1973). Nonparametric Statistical Methods. Wiley, New York.",
  "Holm S (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6(2), 65-70.",
  "Huynh H, Feldt LS (1976). Estimation of the Box correction for degrees of freedom from sample data in randomized block and split-plot designs. Journal of Educational Statistics, 1(1), 69-82. https://doi.org/10.3102/10769986001001069",
  "Kendall MG (1938). A new measure of rank correlation. Biometrika, 30(1-2), 81-93. https://doi.org/10.2307/2332226",
  "Koenker R (1981). A note on studentizing a test for heteroscedasticity. Journal of Econometrics, 17(1), 107-112. https://doi.org/10.1016/0304-4076(81)90062-2",
  "Kramer CY (1956). Extension of multiple range tests to group means with unequal numbers of replications. Biometrics, 12(3), 307-310. https://doi.org/10.2307/3001469",
  "Kruskal WH, Wallis WA (1952). Use of ranks in one-criterion variance analysis. Journal of the American Statistical Association, 47(260), 583-621. https://doi.org/10.1080/01621459.1952.10483441",
  "Mann HB, Whitney DR (1947). On a test of whether one of two random variables is stochastically larger than the other. The Annals of Mathematical Statistics, 18(1), 50-60. https://doi.org/10.1214/aoms/1177730491",
  "Mauchly JW (1940). Significance test for sphericity of a normal n-variate distribution. Annals of Mathematical Statistics, 11(2), 204-209. https://doi.org/10.1214/aoms/1177731915",
  "Pearson K (1895). Notes on regression and inheritance in the case of two parents. Proceedings of the Royal Society of London, 58, 240-242.",
  "Satterthwaite FE (1946). An approximate distribution of estimates of variance components. Biometrics Bulletin, 2(6), 110-114. https://doi.org/10.2307/3002019",
  "Shapiro SS, Wilk MB (1965). An analysis of variance test for normality (complete samples). Biometrika, 52(3-4), 591-611. https://doi.org/10.2307/2333709",
  "Spearman C (1904). The proof and measurement of association between two things. The American Journal of Psychology, 15(1), 72-101. https://doi.org/10.2307/1412159",
  "Student (1908). The probable error of a mean. Biometrika, 6(1), 1-25. https://doi.org/10.2307/2331554",
  "Tukey JW (1949). Comparing individual means in the analysis of variance. Biometrics, 5(2), 99-114. https://doi.org/10.2307/3001913",
  "Welch BL (1947). The generalization of 'Student's' problem when several different population variances are involved. Biometrika, 34(1-2), 28-35. https://doi.org/10.2307/2332510",
  "Welch BL (1951). On the comparison of several mean values: an alternative approach. Biometrika, 38(3-4), 330-336. https://doi.org/10.2307/2332579",
  "Wilcox RR (2017). Introduction to Robust Estimation and Hypothesis Testing, 4th ed. Academic Press, London.",
  "Wilcoxon F (1945). Individual comparisons by ranking methods. Biometrics Bulletin, 1(6), 80-83. https://doi.org/10.2307/3001968",
  "Wilkinson GN, Rogers CE (1973). Symbolic description of factorial models for analysis of variance. Applied Statistics, 22(3), 392-399. https://doi.org/10.2307/2346786",
  "Yuen KK (1974). The two-sample trimmed t for unequal population variances. Biometrika, 61(1), 165-170. https://doi.org/10.1093/biomet/61.1.165",
  "Yates F (1933). The analysis of replicated experiments when the field results are incomplete. Empire Journal of Experimental Agriculture, 1, 129-142.",
  "Zimmerman DW (2004). A note on preliminary tests of equality of variances. British Journal of Mathematical and Statistical Psychology, 57(1), 173-181. https://doi.org/10.1348/000711004849222"
)

# ---- building the topics of the Methods page ----
# the entry of method_refs that starts with `key`, e.g. "Tukey JW (1949)"
method_ref <- function(key) {
  hit <- method_refs[startsWith(method_refs, key)]
  if (length(hit) != 1) {
    stop("No single method reference starts with '", key, "'.", call. = FALSE)
  }
  hit
}
# the R packages a topic uses besides base R, from its "pkg::fun()" entries
topic_pkgs <- function(t) {
  setdiff(
    unique(sub("::.*", "", grep("^[A-Za-z0-9.]+::", t$software, value = TRUE))),
    c("base", "stats", "utils")
  )
}
# R and each package with the version installed now
topic_software <- function(t) {
  if (length(t$software)) {
    c(
      paste("R", paste(R.version$major, R.version$minor, sep = ".")),
      vapply(topic_pkgs(t), function(p) trimws(paste(p, pkg_version(p))), "")
    )
  }
}
# the references of a topic: R and its packages (from citation()) and the methods
topic_refs <- function(t) {
  unname(sort(unique(c(
    if (length(t$software)) package_ref("base"),
    vapply(topic_pkgs(t), package_ref, ""),
    vapply(t$refs, method_ref, "")
  ))))
}
# what the copy button of a topic puts on the clipboard
topic_copy_text <- function(t) {
  paste(
    c(
      t$title,
      if (length(t$software)) {
        c(
          paste("Software:", paste(topic_software(t), collapse = ", ")),
          paste("Functions:", paste(t$software, collapse = "; "))
        )
      },
      "",
      "References",
      topic_refs(t)
    ),
    collapse = "\n"
  )
}
topic_ui <- function(t) {
  refs <- topic_refs(t)
  bslib::accordion_panel(
    t$title,
    value = t$id,
    t$body,
    if (length(t$software)) {
      div(
        class = "small mb-2",
        p(
          class = "mb-1",
          strong("ซอฟต์แวร์ที่ใช้: "),
          paste(topic_software(t), collapse = ", ")
        ),
        p(
          class = "mb-1",
          strong("ฟังก์ชัน: "),
          lapply(seq_along(t$software), function(i) {
            x <- t$software[i]
            tagList(
              if (i > 1) "; ",
              if (endsWith(x, "()") && !grepl(" ", x, fixed = TRUE)) {
                code(x)
              } else {
                x
              }
            )
          })
        )
      )
    },
    if (length(refs)) {
      tagList(
        h6("เอกสารอ้างอิงของหัวข้อนี้"),
        tags$ul(class = "small", lapply(refs, tags$li)),
        actionButton(
          paste0("cp_topic_", t$id),
          tagList(icon("copy"), " คัดลอกซอฟต์แวร์และเอกสารอ้างอิงของหัวข้อนี้"),
          class = "btn-sm"
        )
      )
    }
  )
}
# the topics in their groups, each group an accordion that starts closed
methods_topics_ui <- function() {
  tagList(lapply(names(method_groups), function(g) {
    ts <- Filter(function(t) t$group == g, method_topics)
    tagList(
      h4(class = "mt-4", method_groups[[g]]),
      do.call(
        bslib::accordion,
        c(
          lapply(ts, topic_ui),
          list(id = paste0("methods_", g), open = FALSE, multiple = TRUE)
        )
      )
    )
  }))
}
