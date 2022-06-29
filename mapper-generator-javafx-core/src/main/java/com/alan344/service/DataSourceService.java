package com.alan344.service;

import com.alan344.bean.DataSource;
import com.alan344.constants.BaseConstants;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 21:21
 */
@Slf4j
@Service
public class DataSourceService {

    @Resource
    private TableService tableService;

    @Resource
    private ApplicationContext applicationContext;

    @Resource
    private JdbcTemplate jdbcTemplate;

    /**
     * 用户判断数据源是否重复
     */
    @Getter
    private final Set<DataSource> dataSourceSet = new HashSet<>();

    /**
     * 添加数据源
     *
     * @param dataSource 数据源信息
     * @throws IOException e
     */
    public void addDataSource(DataSource dataSource) throws IOException {
        this.downLoadToFile(dataSource);

        this.addDataSourceToSpring(dataSource);

        dataSourceSet.add(dataSource);
    }

    /**
     * 删除数据源
     *
     * @param dataSource 数据源信息
     */
    public void deleteDataSource(DataSource dataSource) {
        this.deleteDataSourceFile(dataSource);

        tableService.deleteTables(dataSource);

        dataSourceSet.remove(dataSource);
    }

    /**
     * 把datasource信息记录到文件
     */
    @Async
    void downLoadToFile(DataSource dataSource) throws IOException {
        String datasourceStr = JSON.toJSONString(dataSource);

        FileUtils.writeStringToFile(BaseConstants.getDataSourceFile(dataSource), datasourceStr, StandardCharsets.UTF_8.toString());
    }

    /**
     * 把datasource文件从磁盘删除
     */
    @Async
    void deleteDataSourceFile(DataSource dataSource) {
        try {
            FileUtils.forceDelete(BaseConstants.getDataSourceFile(dataSource));
        } catch (IOException e) {
            log.error("删除数据源文件错误", e);
        }
    }

    /**
     * 从文件加载数据源至pane
     */
    public List<DataSource> loadDataSourceFromFile() {
        File file1 = new File(BaseConstants.MG_DATA_HOME);
        if (!file1.exists()) {
            return Collections.emptyList();
        }
        Collection<File> files = FileUtils.listFiles(file1, null, false);
        if (files.isEmpty()) {
            return Collections.emptyList();
        }

        try {
            for (File file : files) {
                if (file.getName().endsWith("datasource")) {
                    DataSource dataSource = JSONObject.parseObject(FileUtils.readFileToString(file, StandardCharsets.UTF_8.toString()), DataSource.class);
                    //从文件加表信息至pane
                    tableService.loadTablesFromFile(dataSource);

                    dataSourceSet.add(dataSource);
                    //向Spring注册dataSource
                    this.addDataSourceToSpring(dataSource);
                }
            }
        } catch (IOException e) {
            log.error("加载dataSource文件失败", e);
        }
        return new ArrayList<>(dataSourceSet);
    }

    /**
     * 注册dataSource至Spring
     *
     * @param dataSource 数据源信息
     */
    private void addDataSourceToSpring(DataSource dataSource) {
        ConfigurableApplicationContext applicationContext = (ConfigurableApplicationContext) this.applicationContext;
        JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource.createDataSource());
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
        jdbcTemplate.setDataSource(dataSource.createDataSource());
        jdbcTemplate.setQueryTimeout(3);
        try {
            jdbcTemplate.query("SELECT 1 FROM dual", (rs, rowNum) -> rs.getString(1));
        } catch (Exception e) {
            return false;
        }
        return true;
    }
}
