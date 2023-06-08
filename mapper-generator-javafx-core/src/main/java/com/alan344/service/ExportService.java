package com.alan344.service;

import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.DialogFactory;
import com.alan344.service.generator.MapperGeneratorStrategyContext;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * @author AlanSun
 * @date 2020/9/11 8:57
 */
@Slf4j
@Service
public class ExportService {
    @Resource
    private MapperGeneratorStrategyContext mapperGeneratorStrategyContext;
    @Resource
    private TableService tableService;
    @Resource
    private ColumnService columnService;
    @Autowired
    private ConfigService configService;

    /**
     * 导出
     */
    public void export(MybatisExportConfig mybatisExportConfig) {
        this.saveSetupInternal(mybatisExportConfig);

        // 调用 mybatis generator 生成文件
        mapperGeneratorStrategyContext.getMapperGeneratorStrategy(mybatisExportConfig).generator(mybatisExportConfig);

        // 弹框
        DialogFactory.successAndOpenFileDialog(NodeConstants.primaryStage, "导出成功", mybatisExportConfig.getProjectDir());
    }

    /**
     * 保存配置
     *
     * @param mybatisExportConfig {@link MybatisExportConfig}
     */
    public void saveSetup(MybatisExportConfig mybatisExportConfig) {
        this.saveSetupInternal(mybatisExportConfig);
    }

    private void saveSetupInternal(MybatisExportConfig mybatisExportConfig) {
        // 写入文件
        configService.addConfig(mybatisExportConfig);

        // 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 table 配置（如 insert）文件重写
        tableService.downLoadTableIfOverrideModify();

        // 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 columns 文件重写
        columnService.downLoadColumnOverride();
    }
}
