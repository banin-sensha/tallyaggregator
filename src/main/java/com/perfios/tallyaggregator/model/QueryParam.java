package com.perfios.tallyaggregator.model;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class QueryParam {
    //collection field name
    private String fieldName;
    //Operator name
    private String operator;
    //value
    private Object value;
}
