package com.perfios.tallyaggregator.restcontroller;

import com.perfios.tallyaggregator.common.GenericResponse;
import com.perfios.tallyaggregator.common.StatusResponse;
import com.perfios.tallyaggregator.common.XMLToJavaObject;
import com.perfios.tallyaggregator.model.OperatorEnum;
import com.perfios.tallyaggregator.model.QueryParam;
import com.perfios.tallyaggregator.repository.CustomRepository;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.bson.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.*;

@AllArgsConstructor
@RestController
@Slf4j
public class SalesController {
    private static final Logger logger = LoggerFactory.getLogger(SalesController.class);

    private final CustomRepository customRepository;

    @Autowired
    private final XMLToJavaObject xmlToJavaObject;

    @RequestMapping(value = {"/sales/{companyName}/{fromDate}/{toDate}"}, method = RequestMethod.GET, produces = "application/json")
    public ResponseEntity<?> getSales(@PathVariable String companyName, @PathVariable String fromDate, @PathVariable String toDate) {
        Document companyDoc = customRepository.getRecord("name", companyName, "t_company_details");

        QueryParam[] queryParams = new QueryParam[3];
        queryParams[0] = new QueryParam("company_id", OperatorEnum.EQ.name(), Long.parseLong(companyDoc.get("_id").toString()));
        queryParams[1] = new QueryParam("date", OperatorEnum.GTE.name(), fromDate);
        queryParams[2] = new QueryParam("date", OperatorEnum.LT.name(), toDate);

        GenericResponse genericResponse = new GenericResponse();
        List<Document> result = null;

        try {
            result = customRepository.search("t_sales", -1, queryParams);
        } catch (Exception e) {
            logger.error("Exception while fetching sales data = {}", e);
            genericResponse.setStatus(StatusResponse.getErrorMessage(HttpStatus.BAD_REQUEST, "Exception while fetching sales data"));
            return ResponseEntity.badRequest().body(genericResponse);
        }

        Map<String, Object> response = new HashMap<>();
        response.put("result", result);

        genericResponse.setData(response);
        genericResponse.setStatus(StatusResponse.getSuccessResponse());
        return ResponseEntity.ok().body(genericResponse);
    }

    @RequestMapping(value={"/sales"}, method = RequestMethod.POST, produces = "application/json")
    public void postSales(@RequestBody String request) throws Exception {

        List<Map<String, Object>> totalSalesPerMonth = xmlToJavaObject.getTotalSalesPerMonth(request, customRepository.getComanyId("Tally on Wheels"));
        logger.info("totalSalesPerMonth = {}", totalSalesPerMonth);

        Collection record = customRepository.insertCollection(totalSalesPerMonth, "t_sales");

        logger.info("Successfully inserted sales = {}", record);
    }
}
