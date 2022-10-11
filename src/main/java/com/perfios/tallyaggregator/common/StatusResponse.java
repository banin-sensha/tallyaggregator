package com.perfios.tallyaggregator.common;

import org.springframework.http.HttpStatus;

import java.util.HashMap;
import java.util.Map;

public class StatusResponse {
    public static Map<String, Object> getSuccessResponse() {
        final Map<String, Object> status = new HashMap<>();
        status.put("message", "success");
        status.put("code", HttpStatus.OK);
        return status;
    }

    public static Map<String, Object> getErrorMessage(HttpStatus httpStatus,String errorMsg) {
        final Map<String, Object> status = new HashMap<>();
        status.put("message", errorMsg);
        status.put("code", httpStatus.value());
        return status;
    }

}
