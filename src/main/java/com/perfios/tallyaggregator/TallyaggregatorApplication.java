package com.perfios.tallyaggregator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;
import org.springframework.web.bind.annotation.RestController;

import static com.mongodb.client.model.Filters.eq;

@SpringBootApplication
@EnableMongoRepositories
@RestController
public class TallyaggregatorApplication {

    @Autowired
    MongoTemplate mongoTemplate;

    public static void main(String[] args) {
        SpringApplication.run(TallyaggregatorApplication.class, args);
    }

//    @GetMapping("/data")
//    public String getData() {
//        String uri = "mongodb://127.0.0.1:27017/?directConnection=true&serverSelectionTimeoutMS=2000&appName=mongosh+1.3.1";
//
//        MongoClient mongoClient = MongoClients.create(uri);
//        MongoDatabase database = mongoClient.getDatabase("tallydb");
//        MongoCollection<Document> collection = database.getCollection("salespermonth");
//
//        Bson projectionFields = Projections.fields(
//                Projections.include("month", "salesAmount"),
//                Projections.excludeId());
//
//        ArrayList<String> list = collection.find()
//                .projection(projectionFields)
//                .map(Document::toJson).into(new ArrayList<>());
//        return new Gson().toJson(list);
//    }
//
//    @GetMapping("/sales")
//    public String getSales() {
//        List<Document> salespermonth = mongoTemplate.findAll(Document.class, "salespermonth");
//
//        List<HashMap<String, Object>> list = new ArrayList<HashMap<String, Object>>();
//        for(Document document: salespermonth) {
//            HashMap<String, Object> map = new HashMap();
//            System.out.println("document" +  document);
//            map.put("month", document.get("month").toString());
//            map.put("amount", document.get("salesAmount").toString());
//            list.add(map);
//        }
//        return new Gson().toJson(list);
//    }
}

