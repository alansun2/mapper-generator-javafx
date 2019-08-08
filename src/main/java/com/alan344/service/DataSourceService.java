package com.alan344.service;

import com.alan344.BaseConstants;
import com.alan344.bean.DataSource;
import com.alan344.utils.TreeUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.Pane;
import org.apache.commons.io.FileUtils;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 21:21
 */

@Service
public class DataSourceService {
    /**
     * 把datasource信息记录到文件
     */
    @Async
    public void downLoadToFile(DataSource dataSource) throws IOException {
        String datasourceStr = JSON.toJSONString(dataSource);

        FileUtils.writeStringToFile(new File(BaseConstants.MG_DATASOURCE_HOME + dataSource.getHost() + "@" + dataSource.getDatabase()), datasourceStr);
    }

    /**
     * 从文件加载数据源至pane
     *
     * @param pane pane
     */
    public void loadDataSourceFromFile(TreeItem<String> pane) {
        Collection<File> files = FileUtils.listFiles(new File(BaseConstants.MG_DATASOURCE_HOME), null, false);
        files.forEach(file -> {
            try {
                DataSource dataSource = JSONObject.parseObject(FileUtils.readFileToString(file), DataSource.class);
                TreeUtils.add2Tree(dataSource.getHost() + "@" + dataSource.getDatabase(), pane);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }
}
