/**
 * สคริปต์สำหรับสร้าง Google Form และ Google Sheet รับข้อเสนอแนะ/แจ้งปัญหาสำหรับ RDstat
 * 
 * วิธีใช้งาน:
 * 1. เปิดเว็บเบราว์เซอร์ เข้าไปที่ https://script.new (ต้องล็อกอินบัญชี Google)
 * 2. ลบโค้ดเดิมในหน้านั้นออก แล้ววางโค้ดชุดนี้ลงไปทั้งหมด
 * 3. กดปุ่ม "บันทึก" (รูปแผ่นดิสก์) แล้วกดปุ่ม "เรียกใช้" (Run / ปุ่ม Play)
 * 4. หาก Google ขอสิทธิ์เข้าถึง (Authorization) ให้กดยอมรับ (Allow)
 * 5. ดูที่ช่อง "บันทึกการดำเนินการ" (Execution log) ด้านล่าง จะได้ลิงก์ Google Form ไปใช้งานได้ทันที
 */

function createRDstatFeedbackForm() {
  // 1. สร้าง Google Form
  var form = FormApp.create('RDstat - แบบฟอร์มส่งข้อเสนอแนะและแจ้งปัญหา');
  
  form.setTitle('RDstat - แบบฟอร์มรับฟังข้อเสนอแนะและแจ้งปัญหาการใช้งาน')
      .setDescription(
        'แบบฟอร์มนี้จัดทำขึ้นเพื่อรวบรวมความคิดเห็น ปัญหาที่พบ (Bug) หรือฟีเจอร์ที่อยากให้เพิ่ม ' +
        'สำหรับโปรแกรม RDstat (Statistical Tools for Agricultural Research - Rice Department)\n\n' +
        'ทีมงานขอขอบคุณทุกความคิดเห็นเพื่อร่วมกันพัฒนาเครื่องมือวิเคราะห์สถิติที่เหมาะสมกับงานวิจัยข้าวมากที่สุดครับ'
      )
      .setAllowResponseEdits(false)
      .setCollectEmail(false);

  // คำถามที่ 1: ประเภทของข้อความ (Multiple choice)
  var itemType = form.addMultipleChoiceItem();
  itemType.setTitle('1. ประเภทของข้อความที่ต้องการแจ้ง')
          .setChoiceValues([
            '💡 เสนอแนะฟีเจอร์ใหม่ / สถิติที่อยากให้เพิ่ม',
            '🐞 รายงานข้อผิดพลาด (Bug / คำนวณไม่ผ่าน / กราฟผิดปกติ)',
            '💬 ข้อเสนอแนะทั่วไป / ความสะดวกในการใช้งาน',
            '❓ สอบถามวิธีการใช้งาน / วิธีแปลผล'
          ])
          .setRequired(true);

  // คำถามที่ 2: หน้าจอหรือฟังก์ชันที่เกี่ยวข้อง (Checkboxes)
  var itemScreen = form.addCheckboxItem();
  itemScreen.setTitle('2. หน้าจอหรือฟังก์ชันที่เกี่ยวข้อง (เลือกได้มากกว่า 1 ข้อ)')
            .setChoiceValues([
              'หน้าแรก (Home)',
              't-test (เปรียบเทียบค่าเฉลี่ย 2 กลุ่ม)',
              'Analysis of variance (ANOVA - CRD, RCBD, Split Plot, Combined)',
              'Correlation (สหสัมพันธ์)',
              'Regression (การถดถอย)',
              'การนำเข้าไฟล์ Excel / CSV หรือการแปลงรูปแบบตาราง',
              'การปรับแต่งกราฟและการบันทึกรูปภาพ (Plot / Live View)',
              'การดาวน์โหลดรายงาน (Excel / Word / TXT)',
              'อื่นๆ'
            ])
            .setRequired(true);

  // คำถามที่ 3: รายละเอียด (Paragraph text)
  var itemDesc = form.addParagraphTextItem();
  itemDesc.setTitle('3. รายละเอียดข้อเสนอแนะ หรือปัญหาที่พบ')
          .setHelpText('โปรดระบุรายละเอียด เช่น ขั้นตอนที่ทำแล้วเกิดปัญหา, ข้อความที่แสดง, หรือลักษณะฟังก์ชันที่อยากได้เพิ่มเติม')
          .setRequired(true);

  // คำถามที่ 4: ข้อมูลวินิจฉัยระบบ (Paragraph text - Optional)
  var itemDiag = form.addParagraphTextItem();
  itemDiag.setTitle('4. ข้อมูลวินิจฉัยระบบ (Diagnostic Info)')
          .setHelpText('หากพบปัญหาในโปรแกรม ให้กดปุ่ม "คัดลอกข้อมูลวินิจฉัย" ใน RDstat แล้วนำข้อความมาวาง (Paste) ในช่องนี้');

  // คำถามที่ 5: ชื่อและช่องทางติดต่อกลับ (Text - Optional)
  var itemContact = form.addTextItem();
  itemContact.setTitle('5. ชื่อผู้แจ้ง / ศูนย์วิจัยข้าว / อีเมลหรือเบอร์โทรติดต่อกลับ (ไม่บังคับ)')
             .setHelpText('เพื่อความสะดวกในการประสานงานสอบถามข้อมูลเพิ่มเติม หรือแจ้งเมื่อแก้ไขปัญหาเรียบร้อย');

  // 2. สร้าง Google Sheet สำหรับเก็บคำตอบอัตโนมัติ
  var sheet = SpreadsheetApp.create('RDstat - บันทึกข้อเสนอแนะและแจ้งปัญหา (Responses)');
  form.setDestination(FormApp.DestinationType.SPREADSHEET, sheet.getId());

  var formUrl = form.getPublishedUrl();
  var editUrl = form.getEditUrl();
  var sheetUrl = sheet.getUrl();

  Logger.log('======================================================================');
  Logger.log('🎉 สร้างแบบฟอร์มสำเร็จเรียบร้อยใน Google Drive ของท่าน!');
  Logger.log('👉 [1] ลิงก์สำหรับให้ผู้ใช้กรอก (นำไปใส่ใน RDstat): ' + formUrl);
  Logger.log('👉 [2] ลิงก์สำหรับท่านแก้ไขแบบฟอร์ม: ' + editUrl);
  Logger.log('👉 [3] ลิงก์ Google Sheet รวบรวมคำตอบ: ' + sheetUrl);
  Logger.log('======================================================================');
}
