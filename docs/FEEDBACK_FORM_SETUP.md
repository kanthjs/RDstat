# การสร้างและเชื่อมต่อ Google Form สำหรับรับข้อเสนอแนะ RDstat

แบบฟอร์มนี้ใช้สำหรับรวบรวมข้อเสนอแนะ รายงานข้อผิดพลาด (Bug) และข้อเสนอแนะฟีเจอร์เพิ่มเติมจากนักวิจัยและผู้ใช้งานโปรแกรม RDstat

---

## วิธีที่ 1: สร้างอัตโนมัติด้วย Google Apps Script (แนะนำ - ใช้เวลา 10 วินาที)

วิธีนี้รวดเร็วที่สุด เพราะระบบของ Google จะสร้างทั้ง **Google Form** และ **Google Sheet เก็บคำตอบ** ให้โดยอัตโนมัติใน Google Drive ของท่าน:

1. เปิดเว็บเบราว์เซอร์ไปที่ **[https://script.new](https://script.new)** (ต้องล็อกอินบัญชี Google ของท่านไว้แล้ว)
2. ลบโค้ดเริ่มต้นทั้งหมดในหน้าต่างนั้นออก
3. คัดลอกโค้ดจากไฟล์ [`scripts/create_feedback_form.gs`](../scripts/create_feedback_form.gs) ไปวางลงในหน้าเว็บ
4. กดปุ่ม **บันทึก** (รูปแผ่นดิสก์ หรือ Ctrl + S) แล้วกดปุ่ม **เรียกใช้ (Run)**
5. หากมีหน้าต่างขอสิทธิ์การเข้าถึง (Authorization Required):
   - กด **Review Permissions**
   - เลือกบัญชี Google ของท่าน
   - กด **Advanced** (ขั้นสูง) ด้านล่างซ้าย แล้วกด **Go to Untitled project (unsafe)**
   - กด **Allow** (อนุญาต)
6. ด้านล่างในช่อง **Execution log** (บันทึกการดำเนินการ) จะแสดง:
   - **ลิงก์สำหรับให้ผู้ใช้กรอก**: นำลิงก์นี้ไปใส่ใน RDstat
   - **ลิงก์ Google Sheet รวบรวมคำตอบ**: ไว้สำหรับทีมงานเปิดดูสรุปความคิดเห็นทั้งหมด

---

## วิธีที่ 2: สร้างด้วยตนเองบน Google Forms

หากต้องการสร้างและจัดเรียงด้วยตนเอง สามารถเข้าไปที่ **[https://forms.new](https://forms.new)** แล้วตั้งค่าดังนี้:

- **ชื่อแบบฟอร์ม**: RDstat - แบบฟอร์มส่งข้อเสนอแนะและแจ้งปัญหา
- **คำอธิบาย**: แบบฟอร์มรับฟังความคิดเห็น รายงานข้อผิดพลาด และเสนอแนะฟีเจอร์เพิ่มเติมสำหรับโปรแกรม RDstat

### รายการคำถาม:
1. **ประเภทของข้อความที่ต้องการแจ้ง** (หลายตัวเลือก / Multiple choice) *จำเป็น*
   - 💡 เสนอแนะฟีเจอร์ใหม่ / สถิติที่อยากให้เพิ่ม
   - 🐞 รายงานข้อผิดพลาด (Bug / คำนวณไม่ผ่าน / กราฟผิดปกติ)
   - 💬 ข้อเสนอแนะทั่วไป / ความสะดวกในการใช้งาน
   - ❓ สอบถามวิธีการใช้งาน / วิธีแปลผล
2. **หน้าจอหรือฟังก์ชันที่เกี่ยวข้อง** (ช่องทำเครื่องหมาย / Checkboxes) *จำเป็น*
   - หน้าแรก (Home)
   - t-test (เปรียบเทียบค่าเฉลี่ย 2 กลุ่ม)
   - Analysis of variance (ANOVA - CRD, RCBD, Split Plot, Combined)
   - Correlation (สหสัมพันธ์)
   - Regression (การถดถอย)
   - การนำเข้าไฟล์ Excel / CSV หรือการแปลงรูปแบบตาราง
   - การปรับแต่งกราฟและการบันทึกรูปภาพ (Plot / Live View)
   - การดาวน์โหลดรายงาน (Excel / Word / TXT)
   - อื่นๆ
3. **รายละเอียดข้อเสนอแนะ หรือปัญหาที่พบ** (ย่อหน้า / Paragraph text) *จำเป็น*
   - คำอธิบาย: โปรดระบุรายละเอียด เช่น ขั้นตอนที่ทำแล้วเกิดปัญหา หรือลักษณะฟังก์ชันที่อยากได้
4. **ข้อมูลวินิจฉัยระบบ (Diagnostic Info)** (ย่อหน้า / Paragraph text) *ไม่บังคับ*
   - คำอธิบาย: หากพบปัญหาในโปรแกรม ให้กดปุ่ม "คัดลอกข้อมูลวินิจฉัย" ใน RDstat แล้วนำมาวางในช่องนี้
5. **ชื่อผู้แจ้ง / ศูนย์วิจัยข้าว / ข้อมูลติดต่อกลับ** (คำตอบสั้นๆ / Short answer) *ไม่บังคับ*

---

## ลิงก์แบบฟอร์มที่ใช้งานในปัจจุบัน (Active Links)

- **ลิงก์สำหรับผู้ใช้งาน (Public Form)**: [https://docs.google.com/forms/d/e/1FAIpQLScjlgyby2p30Czp8L3MDcIyHSQ03c3ja07yieUgIeR79M7PQw/viewform](https://docs.google.com/forms/d/e/1FAIpQLScjlgyby2p30Czp8L3MDcIyHSQ03c3ja07yieUgIeR79M7PQw/viewform)
- **ลิงก์สำหรับผู้ดูแลแก้ไขแบบฟอร์ม (Form Edit)**: [https://docs.google.com/forms/d/1DCKZ-XExJ4hUnkV2qMOfkcf9dD0eAQXrMKHFHTT5BAs/edit](https://docs.google.com/forms/d/1DCKZ-XExJ4hUnkV2qMOfkcf9dD0eAQXrMKHFHTT5BAs/edit)
- **ลิงก์ Google Sheet รวมคำตอบ (Responses Spreadsheet)**: [https://docs.google.com/spreadsheets/d/1s-6mCuhAB9UnF91eKkB1BzEjDhgOCZF6Z9fmClhyVyw/edit](https://docs.google.com/spreadsheets/d/1s-6mCuhAB9UnF91eKkB1BzEjDhgOCZF6Z9fmClhyVyw/edit)

ลิงก์สาธารณะนี้ถูกตั้งค่าเป็นค่าเริ่มต้นใน `prototype/app/app.R` เรียบร้อยแล้ว (สามารถ override ผ่าน environment variable `RDSTAT_FEEDBACK_URL` ได้หากต้องการเปลี่ยนในอนาคต)
