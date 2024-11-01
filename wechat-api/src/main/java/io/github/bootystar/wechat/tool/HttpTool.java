package io.github.bootystar.wechat.tool;


import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * http请求工具类
 * @author bootystar
 *
 */
public class HttpTool {

    public  final static String METHOD_GET="GET";
    public  final static String METHOD_POST="POST";
    public  final static String METHOD_PUT="PUT";
    public  final static String METHOD_DELETE="DELETE";


    public static String createUrlParams(Map<?,?> args){
        if (args != null && !args.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append("?");
            Iterator<? extends Map.Entry<?, ?>> it = args.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry<?, ?> next = it.next();
                sb.append(next.getKey()).append("=").append(next.getValue()).append("&");
            }
            return sb.substring(0, sb.length() - 1);
        }
        return "";
    }


    public static String doGet(String httpUrl) {
        HttpURLConnection connection = null;
        try {
            // 创建远程url连接对象
            URL url = new URL(httpUrl);
            // 通过远程url连接对象打开一个连接，强转成httpURLConnection类
            connection = (HttpURLConnection) url.openConnection();
            // 设置超时时间：毫秒
            connection.setConnectTimeout(3000);
            // 设置连接请求方式
            connection.setRequestMethod("GET");

            // 发送请求
            connection.connect();
            // 通过connection连接，获取输入流
            if (connection.getResponseCode() == 200) {

                // 封装输入流is，并指定字符集
                try (InputStream is = connection.getInputStream();
                     BufferedReader  br = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
                ){
                    StringBuilder sbf = new StringBuilder();
                    String temp;
                    while ((temp = br.readLine()) != null) {
                        sbf.append(temp);
                        sbf.append("\r\n");
                    }
                    return sbf.toString();
                }
            }else{
                String message= "http: server response failed , \ncode:"+connection.getResponseCode()+"\nmessage:"+connection.getResponseMessage();
                throw new RuntimeException(message);
            }
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage());
        } finally {
            // 关闭资源
            if (connection != null) {
                connection.disconnect();// 关闭远程连接
            }
        }
    }




    public static String doPostForm(String httpUrl, String param) {
        HashMap<String, String> map = new HashMap<>();
        map.put("Content-Type", "application/x-www-form-urlencoded");
        return doRequest(httpUrl,METHOD_POST,map,param,3000);
    }


    public static String doPostJson(String httpUrl, String json) {
        HashMap<String, String> map = new HashMap<>();
        map.put("Content-Type","application/json; charset=UTF-8");
        map.put("accept", "application/json");
        return doRequest(httpUrl,METHOD_POST,map,json,3000);
    }



    private static String doRequest(String httpUrl, String method ,Map<?,?> requestProperties, String body,int timeOut){
        HttpURLConnection connection = null;
        try {
            URL url = new URL(httpUrl);
            // 通过远程url连接对象打开连接
            connection = (HttpURLConnection) url.openConnection();
            // 设置连接请求方式
            connection.setRequestMethod(method);
            // 设置超时时间： 毫秒
            connection.setConnectTimeout(timeOut);

            // 默认值为：false，当向远程服务器传送数据/写数据时，需要设置为true
            connection.setDoOutput(true);
            // 默认值为：true，当前向远程服务读取数据时，设置为true，该参数可有可无
            connection.setDoInput(true);

            // 设置传入参数的格式(Content-Type等):请求参数应该是 name1=value1&name2=value2 的形式。
            if (requestProperties!=null&&!requestProperties.isEmpty()){
                for (Map.Entry<?, ?> entry : requestProperties.entrySet()) {
                    connection.setRequestProperty(entry.getKey().toString(),entry.getValue().toString());
                }
            }

            // 通过连接对象获取一个输出流
            if (body==null){
                body="";
            }
            try (OutputStream os = connection.getOutputStream();){
                // 通过输出流对象将参数写出去/传输出去,它是通过字节数组写出的
                os.write(body.getBytes());
                // 通过连接对象获取一个输入流，向远程读取
                if (connection.getResponseCode() == 200) {
                    // 封装输入流is，并指定字符集
                    try (InputStream is = connection.getInputStream();
                         BufferedReader  br = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
                    ){
                        StringBuilder sbf = new StringBuilder();
                        String temp;
                        while ((temp = br.readLine()) != null) {
                            sbf.append(temp);
                            sbf.append("\r\n");
                        }
                        return sbf.toString();
                    }
                }else{
                    String message= "http: server response failed , \ncode:"+connection.getResponseCode()+"\nmessage:"+connection.getResponseMessage();
                    throw new RuntimeException(message);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage());
        } finally {
            // 关闭资源
            if (connection != null) {
                connection.disconnect();// 关闭远程连接
            }
        }
    }


}
