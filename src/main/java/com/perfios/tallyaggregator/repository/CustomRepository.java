package com.perfios.tallyaggregator.repository;

import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.Filters;
import com.perfios.tallyaggregator.model.QueryParam;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

@Component
public class CustomRepository {

    @Autowired
    MongoTemplate mongoTemplate;

    public Document insertAndReturn(String jsonData, String collectionName) {
        Document doc = Document.parse(jsonData);
        return mongoTemplate.insert(doc, collectionName);
    }

    public Collection insertCollection(Collection jsonData, String collectionName) {
        return mongoTemplate.insert(jsonData, collectionName);
    }

    public Document update(String jsonData, String collectionName) {
        Document doc = Document.parse(jsonData);
        return mongoTemplate.save(doc, collectionName);
    }

    public Document update(Document data, String collectionName) {
        return mongoTemplate.save(data, collectionName);
    }

    /**
     *
     * To be used by caution as fetching whole data then matching using filter key val
     */
    public Document  getRecord(String key,String value, String collectionName){
        return mongoTemplate.getCollection(collectionName).find(Filters.eq(key,value)).first();
    }

    /**
     *
     * To be used by caution as fetching whole data then matching using filter key val
     */
    public Document  getRecord(String key,Long value, String collectionName){
        return mongoTemplate.getCollection(collectionName).find(Filters.eq(key,value)).first();
    }

    public List<Document> findAll(String collectionName) throws Exception {
        return  mongoTemplate.findAll(Document.class,collectionName);
    }

    /*
     *//**
     *
     * Search document
     *
     * Use this method only when having unique fieldName in queryParams
     *
     * @param collectionName:
     *            Collection name
     * @param qp
     *            : Query Parameters
     * @return list of found documents
     * @throws Exception -> TODO change this to custom exception
     *//*
    public List<Document> search(String collectionName, QueryParam... qp) throws Exception {
        try {
            MongoCollection<Document> collection = mongoTemplate.getCollection(collectionName);
            List<Document> result = new ArrayList<>();
            Document query = new Document();
            Arrays.asList(qp).stream().forEach(p -> {
                String operator = p.getOperator();
                String fieldName = p.getFieldName();
                Object value = p.getValue();
                switch (operator) {
                    case "EQ":
                        query.append(fieldName, new Document().append("$eq", value));
                        break;
                    case "NE":
                        query.append(fieldName, new Document().append("$ne", value));
                        break;
                    case "GT":
                        query.append(fieldName, new Document().append("$gt", value));
                        break;
                    case "GTE":
                        query.append(fieldName, new Document().append("$gte", value));
                        break;
                    case "LT":
                        query.append(fieldName, new Document().append("$lt", value));
                        break;
                    case "LTE":
                        query.append(fieldName, new Document().append("$lte", value));
                        break;
                    case "IN":
                        query.append(fieldName, new Document().append("$in", value));
                        break;
                    case "NIN":
                        query.append(fieldName, new Document().append("$nin", value));
                        break;
                    default:
                        break;
                }

            });


            FindIterable<Document> iterable = collection.find(query);
            if (iterable.first() == null) {
                throw new Exception("Record Not Exist");

            }
            for (Document document : iterable) {
                document.remove("_class");
                document.remove("_id");
                result.add(document);
            }


            return result;

        } catch (Exception e) {
            throw new Exception("Something went wrong");
        }
    }*/

    /**
     *
     * Search document
     *
     * Use this method  when having duplicate fieldName in queryParams
     *
     * If pagination is not required pass -1     *
     * @param collectionName:
     *            Collection name
     * @param qp
     *            : Query Parameters
     * @return list of found documents
     * @throws Exception -> TODO change this to custom exception
     */
    public List<Document> search(String collectionName, int pageNo, QueryParam... qp) throws Exception {
        try {
            MongoCollection<Document> collection = mongoTemplate.getCollection(collectionName);
            List<Document> result = new ArrayList<>();
            List<Bson> filters = new ArrayList<Bson>();
            Arrays.asList(qp).stream().forEach(p -> {
                String operator = p.getOperator();
                String fieldName = p.getFieldName();
                Object value = p.getValue();
                switch (operator) {
                    case "EQ":
                        filters.add(Filters.eq(fieldName,value));
                        break;
                    case "NE":
                        filters.add(Filters.ne(fieldName,value));
                        break;
                    case "GT":
                        filters.add(Filters.gt(fieldName,value));
                        break;
                    case "GTE":
                        filters.add(Filters.gte(fieldName,value));
                        break;
                    case "LT":
                        filters.add(Filters.lt(fieldName,value));
                        break;
                    case "LTE":
                        filters.add(Filters.lte(fieldName,value));
                        break;
                    case "IN":
                        filters.add(Filters.in(fieldName,value));
                        break;
                    case "NIN":
                        filters.add(Filters.nin(fieldName,value));
                        break;
                    default:
                        break;
                }

            });
            FindIterable<Document> iterable=null;
            if(pageNo>=0){
                //Need to refine this as this will be slow in performance when the data set is huge
                //Instead of this we can use _id to fetch next data set, need to store last _id in cache or need to be passed as param from api
                iterable = collection.find(Filters.and(filters)).skip(100*(pageNo-1)).limit(100);
            }
            else {
                iterable = collection.find(Filters.and(filters));
            }

            if (iterable.first() == null) {
                throw new Exception("Record Not Exist");

            }
            for (Document document : iterable) {
                document.remove("_class");
                document.remove("_id");
                result.add(document);
            }


            return result;

        } catch (Exception e) {
            throw new Exception("Something went wrong");
        }
    }

    public void insert(String data,String collectionName) throws Exception{
        try {
            mongoTemplate.insert(data,collectionName);
        }
        catch(Exception e){
            throw new Exception("Cannot Insert Record");
        }

    }

    public int getComanyId(String companyName) throws Exception {
        List<Document> companyDetails = findAll("t_company_details");

        Document companyDetail = companyDetails.stream().filter((company) -> company.get("name").equals(companyName)).collect(Collectors.toList()).get(0);
        return Integer.parseInt(companyDetail.get("_id").toString());
    }
}
