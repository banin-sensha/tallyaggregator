package com.perfios.tallyaggregator.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;

@Component
@Scope("singleton")
public class XMLToJavaObject {

    @Autowired
    XMLToJSON xmlToJSON;

    public List<Map<String, Object>> getCompanyDetails(String request, int latestId){
        Map<String,Object> map = xmlToJSON.xmlToJson(request);

        Map<String,Object> envelope = (Map<String, Object>) map.get("ENVELOPE");
        Map<String,Object> body = (Map<String, Object>) envelope.get("BODY");
        Map<String,Object> data = (Map<String, Object>) body.get("DATA");
        Map<String,Object> collection = (Map<String, Object>) data.get("COLLECTION");

        List<Map<String, Object>> companies = new ArrayList<>();



        //If there is only single company is open
        if(collection.get("COMPANY") instanceof Map<?,?>){
            Map<String, Object> outCompany = new HashMap<>();
            Map<String,Object> company = (Map<String, Object>) collection.get("COMPANY");

            outCompany.put("_id", ++latestId);
            if(company.containsKey("NAME")) {
                List<String> companyName = (List<String>) company.get("NAME");
                outCompany.put("name", companyName.get(0));
            }
            else {
                outCompany.put("name", "");
            }

            if(company.containsKey("REMOTEFULLLISTNAME")) {
                Map<String, String> companyAddress = (Map<String, String>) company.get("REMOTEFULLLISTNAME");
                outCompany.put("address", companyAddress.get("content"));
            }
            else {
                outCompany.put("address", "");
            }

            if(company.containsKey("EMAIL")) {
                Map<String, String> companyEmailId = (Map<String, String>) company.get("EMAIL");
                outCompany.put("email", companyEmailId.get("content"));
            }
            else {
                outCompany.put("email", "");
            }

            if(company.containsKey("MOBILENO")) {
                Map<String, Double> companyMobileNo = (Map<String, Double>) company.get("MOBILENO");
                outCompany.put("mob_no", String.valueOf(new BigDecimal(companyMobileNo.get("content")).longValue()));
            }
            else {
                outCompany.put("mob_no", "");
            }

            if(company.containsKey("WEBSITE")) {
                Map<String, String> companyWebsite = (Map<String, String>) company.get("WEBSITE");
                outCompany.put("website", companyWebsite.get("content"));
            }
            else {
                outCompany.put("website", "");
            }

            outCompany.put("created_at", new Date());
            outCompany.put("updated_at", new Date());
            companies.add(outCompany);
        }

        //If more than one company is open at the same time
        if(collection.get("COMPANY") instanceof List<?>) {
            List<Map<String, Object>> companyList = (List<Map<String, Object>>) collection.get("COMPANY");
            for (Map<String,Object> company : companyList) {
                Map<String, Object> outCompany = new HashMap<>();

                outCompany.put("_id", ++latestId);
                if(company.containsKey("NAME")) {
                    List<String> companyName = (List<String>) company.get("NAME");
                    outCompany.put("name", companyName.get(0));
                }
                else {
                    outCompany.put("name", "");
                }

                if(company.containsKey("REMOTEFULLLISTNAME")) {
                    Map<String, String> companyAddress = (Map<String, String>) company.get("REMOTEFULLLISTNAME");
                    outCompany.put("address", companyAddress.get("content"));
                }
                else {
                    outCompany.put("address", "");
                }

                if(company.containsKey("EMAIL")) {
                    Map<String, String> companyEmailId = (Map<String, String>) company.get("EMAIL");
                    outCompany.put("email", companyEmailId.get("content"));
                }
                else {
                    outCompany.put("email", "");
                }

                if(company.containsKey("MOBILENO")) {
                    Map<String, Double> companyMobileNo = (Map<String, Double>) company.get("MOBILENO");
                    outCompany.put("mob_no", String.valueOf(new BigDecimal(companyMobileNo.get("content")).longValue()));
                }
                else {
                    outCompany.put("mob_no", "");
                }

                if(company.containsKey("WEBSITE")) {
                    Map<String, String> companyWebsite = (Map<String, String>) company.get("WEBSITE");
                    outCompany.put("website", companyWebsite.get("content"));
                }
                else {
                    outCompany.put("website", "");
                }

                outCompany.put("created_at", new Date());
                outCompany.put("updated_at", new Date());
                companies.add(outCompany);
            }
        }

        return companies;
    }

    public List<Map<String, Object>> getTotalSalesPerMonth(String request, int companyId) {

        Map<String,Object> map = xmlToJSON.xmlToJson(request);

        Map<String, Object> envelope = (Map<String, Object>)map.get("ENVELOPE");
        List<Map<String, Object>> dspaccinfo = (List<Map<String, Object>>)envelope.get("DSPACCINFO");
        List<Object> dates = (List<Object>) envelope.get("DSPPERIOD");

        List<Map<String, Object>> sales = new ArrayList<>();

        int index = 0;

        for (Map<String,Object> accInfo: dspaccinfo) {
            Map<String, Object> sale = new HashMap<>();
            Map<String, Object> clampt= (Map<String, Object>) accInfo.get("DSPCLAMT");
            Map<String, Object> crampt= (Map<String, Object>) accInfo.get("DSPCRAMT");

            String closingAmt = String.valueOf(clampt.get("DSPCLAMTA"));
            if (closingAmt.isEmpty()) {
                sale.put("closing_amount", 0.0);
            }
            else {
                sale.put("closing_amount", Math.abs(Double.parseDouble(closingAmt)));
            }

            String creditAmt = String.valueOf(crampt.get("DSPCRAMTA"));
            if (creditAmt.isEmpty()) {
                sale.put("sale_amount", 0.0);
            }
            else {
                sale.put("sale_amount", Math.abs(Double.parseDouble(creditAmt)));
            }

            sale.put("date", dates.get(index));
            sale.put("_id", index + 1);
            sale.put("company_id", companyId);
            sale.put("created_at", new Date());
            sale.put("updated_at", new Date());

            index ++;

            sales.add(sale);
        }

        return sales;
    }

    public List<Map<String, Object>> getTotalExpensesPerMonth(String request, int companyId) {

        Map<String,Object> map = xmlToJSON.xmlToJson(request);

        Map<String, Object> envelope = (Map<String, Object>)map.get("ENVELOPE");
        List<Map<String, Object>> dspaccinfo = (List<Map<String, Object>>)envelope.get("DSPACCINFO");
        List<Object> dates = (List<Object>) envelope.get("DSPPERIOD");

        List<Map<String, Object>> expenses = new ArrayList<>();

        int index = 0;

        for (Map<String,Object> accInfo: dspaccinfo) {
            Map<String, Object> expense = new HashMap<>();
            Map<String, Object> clampt= (Map<String, Object>) accInfo.get("DSPCLAMT");
            Map<String, Object> crampt= (Map<String, Object>) accInfo.get("DSPDRAMT");

            String closingAmt = String.valueOf(clampt.get("DSPCLAMTA"));
            if (closingAmt.isEmpty()) {
                expense.put("closing_amount", 0.0);
            }
            else {
                expense.put("closing_amount", Math.abs(Double.parseDouble(closingAmt)));
            }

            String debitAmt = String.valueOf(crampt.get("DSPDRAMTA"));
            if (debitAmt.isEmpty()) {
                expense.put("expense_amount", 0.0);
            }
            else {
                expense.put("expense_amount", Math.abs(Double.parseDouble(debitAmt)));
            }

            expense.put("date", dates.get(index));
            expense.put("_id", index + 1);
            expense.put("company_id", companyId);
            expense.put("created_at", new Date());
            expense.put("updated_at", new Date());

            index ++;

            expenses.add(expense);
        }

        return expenses;
    }

    public List<Map<String, Object>> accountReceiveOrPay(String request, int companyId) {

        Map<String,Object> map = xmlToJSON.xmlToJson(request);

        Map<String, Object> envelope = (Map<String, Object>) map.get("ENVELOPE");
        List<Map<String, Object>> billFixedList = (List<Map<String, Object>>) envelope.get("BILLFIXED");
        List<Object> cllList = (List<Object>) envelope.get("BILLCL");

        List<Map<String, Object>> accountBills = new ArrayList<>();

        int index = 0;
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
        for (Map<String, Object> billFixed : billFixedList) {
            Map<String, Object> accountBill = new HashMap<>();

            String billParty = billFixed.get("BILLPARTY").toString();
            accountBill.put("buyer_name", billParty);

            String date = billFixed.get("BILLDATE").toString();
            accountBill.put("invoice_date", formatter.format(new Date(date)).toString());

            String billRef = billFixed.get("BILLREF").toString();
            accountBill.put("invoice_number", billRef);

            String billClString = cllList.get(index).toString();
            double billCl = billClString.isEmpty()? 0: Math.abs(Double.parseDouble(billClString));
            accountBill.put("amount", billCl);

            accountBill.put("_id", index + 1);
            accountBill.put("company_id", companyId);
            accountBill.put("created_at", new Date());
            accountBill.put("updated_at", new Date());

            index ++;

            accountBills.add(accountBill);
        }

        return accountBills;
    }
}
