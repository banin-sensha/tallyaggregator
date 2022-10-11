package com.perfios.tallyaggregator.common;

import lombok.Data;

import java.util.Map;

@Data
public class GenericResponse {
    private Map<String, Object> data;
    private Map<String, Object> status;
}
