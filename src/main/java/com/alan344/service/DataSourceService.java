package com.alan344.service;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.TreeUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.zaxxer.hikari.HikariDataSource;
import javafx.scene.control.TreeItem;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 21:21
 */
@Slf4j
@Service
public class DataSourceService {

    @Autowired
    private MainService mainService;

    @Autowired
    private TableService tableService;

    @Autowired
    private ApplicationContext applicationContext;

    /**
     * 添加数据源
     *
     * @param dataSource 数据源信息
     * @throws IOException e
     */
    public void addDataSource(DataSource dataSource) throws IOException {
        this.downLoadToFile(dataSource);

        this.addDataSourceToSpring(dataSource);
    }

    /**
     * 删除数据源
     *
     * @param dataSource 数据源信息
     */
    public void deleteDataSource(DataSource dataSource) {
        this.deleteDataSourceFile(dataSource);

        tableService.deleteTable(dataSource);
    }

    /**
     * 把datasource信息记录到文件
     */
    @Async
    public void downLoadToFile(DataSource dataSource) throws IOException {
        String datasourceStr = JSON.toJSONString(dataSource);

        FileUtils.writeStringToFile(BaseConstants.getDataSourceFile(dataSource), datasourceStr);
    }

    /**
     * 把datasource文件从磁盘删除
     */
    @Async
    public void deleteDataSourceFile(DataSource dataSource) {
        try {
            FileUtils.forceDelete(BaseConstants.getDataSourceFile(dataSource));
        } catch (IOException e) {
            log.error("删除数据源文件错误", e);
        }
    }

    /**
     * 从文件加载数据源至pane
     *
     * @param treeItemRoot treeItemRoot
     */
    public void loadDataSourceFromFile(TreeItem<DataItem> treeItemRoot) {
        File file1 = new File(BaseConstants.MG_DATA_HOME);
        if (!file1.exists()) {
            return;
        }
        Collection<File> files = FileUtils.listFiles(file1, null, false);
        if (files.isEmpty()) {
            return;
        }

        files.forEach(file -> {
            try {
                if (file.getName().endsWith("datasource")) {
                    DataSource dataSource = JSONObject.parseObject(FileUtils.readFileToString(file), DataSource.class);
                    TreeItem<DataItem> dataSourceItemTreeItem = mainService.add2Tree(dataSource, treeItemRoot);

                    //从文件加表信息至pane
                    boolean isTableEmpty = tableService.loadTablesFromFile(dataSourceItemTreeItem);
                    if (!isTableEmpty) {
                        //下面个没啥用，填充table，让界面看前来有一个下拉箭头，可能会在loadTables方法中删除该item
                        TreeUtils.add2Tree(new Table(), dataSourceItemTreeItem);
                    }
                    //向Spring注册dataSource TODO 可以直接注册jdbcTemplate
                    this.addDataSourceToSpring(dataSource);
                }
            } catch (IOException e) {
                log.error("加载dataSource文件失败", e);
            }
        });
    }

    /**
     * 注册dataSource至Spring
     *
     * @param dataSource 数据源信息
     */
    private void addDataSourceToSpring(DataSource dataSource) {

        ConfigurableApplicationContext applicationContext = (ConfigurableApplicationContext) this.applicationContext;
        HikariDataSource hikariDataSource = new HikariDataSource();
        hikariDataSource.setUsername(dataSource.getUser());
        hikariDataSource.setPassword(dataSource.getPassword());
        hikariDataSource.setJdbcUrl("jdbc:mysql://" + dataSource.getHost() + ":" + dataSource.getPort() + "/" + dataSource.getDatabase() + "?useUnicode=true&characterEncoding=utf-8&autoReconnect=true&useSSL=false&serverTimezone=CTT");
        hikariDataSource.setDriverClassName(dataSource.getDriveName());
        JdbcTemplate jdbcTemplate = new JdbcTemplate(hikariDataSource);
        if (!applicationContext.containsBean(dataSource.toString())) {
            applicationContext.getBeanFactory().registerSingleton(dataSource.toString(), jdbcTemplate);
        }
    }

    /**
     * 测试数据源
     *
     * @param dataSource 数据源
     * @return true 成功 false 失败
     */
    public boolean testConnection(DataSource dataSource) {
        HikariDataSource hikariDataSource = new HikariDataSource();
        hikariDataSource.setUsername(dataSource.getUser());
        hikariDataSource.setPassword(dataSource.getPassword());
        hikariDataSource.setJdbcUrl("jdbc:mysql://" + dataSource.getHost() + ":" + dataSource.getPort() + "/" + dataSource.getDatabase() + "?useUnicode=true&characterEncoding=utf-8&autoReconnect=true&useSSL=false&serverTimezone=CTT");
        hikariDataSource.setDriverClassName(dataSource.getDriveName());
        hikariDataSource.setConnectionTimeout(250);
        JdbcTemplate jdbcTemplate = applicationContext.getBean(JdbcTemplate.class);
        jdbcTemplate.setDataSource(hikariDataSource);

        try {
            jdbcTemplate.query("SELECT 1", (rs, rowNum) -> rs.getString(1));
        } catch (Exception e) {
            return false;
        }
        return true;
    }
}
