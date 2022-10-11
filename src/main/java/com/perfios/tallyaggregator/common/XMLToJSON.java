package com.perfios.tallyaggregator.common;

import com.google.gson.Gson;
import org.json.JSONObject;
import org.json.XML;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.LinkedHashMap;
import java.util.Map;

@Component
@Scope("singleton")
public class XMLToJSON {
    public static Map<String,Object> xmlToJson(String xml){

        JSONObject jsonObject = XML.toJSONObject(xml);
        String jsonString = jsonObject.toString();

        // checks whether jsonString is empty or not
        if (jsonString == null || jsonString.isEmpty()) {
            return null;
        }
        final Gson gson = new Gson();
        Map<String, Object> map = new LinkedHashMap<>();
        map = (Map<String, Object>) gson.fromJson(jsonString, map.getClass());
        return map;
    }
}
