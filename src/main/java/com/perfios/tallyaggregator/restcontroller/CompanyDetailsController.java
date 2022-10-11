package com.perfios.tallyaggregator.restcontroller;

import com.perfios.tallyaggregator.common.GenericResponse;
import com.perfios.tallyaggregator.common.StatusResponse;
import com.perfios.tallyaggregator.common.XMLToJavaObject;
import com.perfios.tallyaggregator.repository.CustomRepository;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.bson.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.*;

@RestController
@AllArgsConstructor
@Slf4j
public class CompanyDetailsController {
    private static final Logger logger = LoggerFactory.getLogger(CompanyDetailsController.class);

    private final CustomRepository customRepository;

    @Autowired
    private final XMLToJavaObject xmlToJavaObject;

    @RequestMapping(value={"/company/details/{companyName}"}, method = RequestMethod.GET, produces = "application/json")
    public ResponseEntity<?> getCompanyDetails(@PathVariable String companyName) {
        Document record = customRepository.getRecord("name", companyName, "t_company_details");

        GenericResponse genericResponse = new GenericResponse();
        Map<String, Object> map = new HashMap<>();
        map.put("result", record);
        genericResponse.setData(map);
        genericResponse.setStatus(StatusResponse.getSuccessResponse());

        return ResponseEntity.ok().body(genericResponse);
    }

    @RequestMapping(value={"/company/details"}, method = RequestMethod.POST, produces = "application/json")
    public void postCompanyDetails(@RequestBody String request) throws Exception {
        List<Document> docs = customRepository.findAll("t_company_details");

        Document latestId = docs.stream().max((x, y) -> Integer.parseInt(x.get("_id").toString()) - Integer.parseInt(y.get("_id").toString())).get();

        List<Map<String, Object>> companyDetails = xmlToJavaObject.getCompanyDetails(request, Integer.parseInt(latestId.get("_id").toString()));
        Collection record = customRepository.insertCollection(companyDetails, "t_company_details");

        logger.info("Successfully inserted company details = {}", record);
    }
}
