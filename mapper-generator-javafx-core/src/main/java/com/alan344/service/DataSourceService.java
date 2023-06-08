package com.alan344.service;

import com.alan344.bean.DataSource;
import com.alan344.constants.BaseConstants;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
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

    /**
     * 用户判断数据源是否重复
     */
    @Getter
    private final Set<DataSource> dataSourceSet = new LinkedHashSet<>();

    /**
     * 添加数据源
     *
     * @param dataSource 数据源信息
     * @throws IOException e
     */
    public void addDataSource(DataSource dataSource) throws IOException {
        this.downLoadToFile(dataSource);

        dataSourceSet.add(dataSource);
    }

    /**
     * 是否存在数据源
     *
     * @param dataSource {@link DataSource}
     * @return true:存在;false:不存在
     */
    public boolean contains(DataSource dataSource) {
        return this.dataSourceSet.contains(dataSource);
    }

    /**
     * 修改数据源
     *
     * @param newDataSource 数据源信息
     * @throws IOException e
     */
    public void updateDataSource(DataSource oldDataSource, DataSource newDataSource) throws IOException {
        if (oldDataSource.isSame(newDataSource)) {
            return;
        }

        // 删除旧的数据源文件
        this.deleteDataSourceFile(oldDataSource);

        // 保存配置
        this.downLoadToFile(newDataSource);

        // 如果配置名或者url不同，需要删除表信息
        if (!oldDataSource.getConfigName().equals(newDataSource.getConfigName()) || !oldDataSource.getUrl().equals(newDataSource.getUrl())) {
            tableService.deleteTables(oldDataSource);
        }

        // 如果url不同，需要重新创建数据源
        if (!oldDataSource.getUrl().equals(newDataSource.getUrl())) {
            newDataSource.closeDataSource();
            newDataSource.setDataSource(null);
            newDataSource.setTables(null);
        }
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
        File file1 = new File(BaseConstants.DATASOURCE_DIR);
        if (!file1.exists()) {
            return Collections.emptyList();
        }
        Collection<File> files = FileUtils.listFiles(file1, null, false);
        if (files.isEmpty()) {
            return Collections.emptyList();
        }

        List<DataSource> result = new ArrayList<>();
        try {
            for (File file : files) {
                DataSource dataSource = JSONObject.parseObject(FileUtils.readFileToString(file, StandardCharsets.UTF_8.toString()), DataSource.class);
                if (dataSourceSet.add(dataSource)) {
                    result.add(dataSource);
                }
            }
        } catch (IOException e) {
            log.error("加载dataSource文件失败", e);
        }
        return result;
    }

    /**
     * 测试数据源
     *
     * @param dataSource 数据源
     * @return true 成功 false 失败
     */
    public boolean testConnection(DataSource dataSource) {
        try (final Connection connection = dataSource.getDataSource().getConnection()) {
            connection.getAutoCommit();
        } catch (Exception e) {
            return false;
        }
        return true;
    }
}
