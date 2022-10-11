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

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@AllArgsConstructor
@RestController
@Slf4j
public class ReceivablesController {
    private static final Logger logger = LoggerFactory.getLogger(ReceivablesController.class);

    private final CustomRepository customRepository;

    @Autowired
    private final XMLToJavaObject xmlToJavaObject;

    @RequestMapping(value = {"/receivables/{companyName}"}, method = RequestMethod.GET, produces = "application/json")
    public ResponseEntity<?> getReceivables(@PathVariable String companyName) {
        Document companyDoc = customRepository.getRecord("name", companyName, "t_company_details");

        QueryParam[] queryParams = new QueryParam[1];
        queryParams[0] = new QueryParam("company_id", OperatorEnum.EQ.name(), Long.parseLong(companyDoc.get("_id").toString()));

        GenericResponse genericResponse = new GenericResponse();
        List<Document> result = null;

        try {
            result = customRepository.search("t_account_receivables", -1, queryParams);
        } catch (Exception e) {
            logger.error("Exception while fetching receivables data = {}", e);
            genericResponse.setStatus(StatusResponse.getErrorMessage(HttpStatus.BAD_REQUEST, "Exception while fetching receivables data"));
            return ResponseEntity.badRequest().body(genericResponse);
        }

        Map<String, Object> response = new HashMap<>();
        response.put("result", result);

        genericResponse.setData(response);
        genericResponse.setStatus(StatusResponse.getSuccessResponse());
        return ResponseEntity.ok().body(genericResponse);
    }

    @RequestMapping(value={"/account/receivables"}, method = RequestMethod.POST, produces = "application/json")
    public void postExpenses(@RequestBody String request) throws Exception {

        List<Map<String, Object>> accountReceivables = xmlToJavaObject.accountReceiveOrPay(request, customRepository.getComanyId("Tally on Wheels"));
        Collection record = customRepository.insertCollection(accountReceivables, "t_account_receivables");

        logger.info("Successfully inserted account receivables = {}", record);
    }
}
