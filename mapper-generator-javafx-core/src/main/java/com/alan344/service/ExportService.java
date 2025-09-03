package com.alan344.service;

import cn.hutool.core.util.StrUtil;
import com.alan344.bean.Table;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.DialogFactory;
import com.alan344.service.generator.MapperGeneratorStrategyContext;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * @author AlanSun
 * @date 2020/9/11 8:57
 */
@Slf4j
@Service
public class ExportService {
    @Autowired
    private MapperGeneratorStrategyContext mapperGeneratorStrategyContext;
    @Autowired
    private TableService tableService;
    @Autowired
    private ColumnService columnService;
    @Autowired
    private ConfigService configService;

    /**
     * 导出
     */
    public void export(MybatisExportConfig mybatisExportConfig) {
        this.saveSetupInternal();

        // 整理每个文件的包名，用于模板文件的导入
        Map<String, String> nameImportMap = new HashMap<>(64);
        Collection<Table> tables = BaseConstants.selectedTableNameTableMap.values();
        for (Table table : tables) {
            final String camelCaseTableName = StrUtil.upperFirst(StrUtil.toCamelCase(table.getTableName()));
            if (ConfigConstants.extraTemplateFileConfigs != null) {
                for (ExtraTemplateFileConfig extraTemplateFileConfig : ConfigConstants.extraTemplateFileConfigs) {
                    nameImportMap.put(camelCaseTableName + extraTemplateFileConfig.getModelSuffix(),
                            extraTemplateFileConfig.getPackageName() + "." + camelCaseTableName + extraTemplateFileConfig.getModelSuffix());
                }

                nameImportMap.put(camelCaseTableName, mybatisExportConfig.getBeanPackage() + "." + camelCaseTableName);
                nameImportMap.put(camelCaseTableName + "Example",
                        mybatisExportConfig.getBeanPackage() + "." + camelCaseTableName + "Example");
                nameImportMap.put(camelCaseTableName + "Mapper",
                        mybatisExportConfig.getMapperPackage() + "." + camelCaseTableName + "Mapper");
                nameImportMap.put(camelCaseTableName + "DynamicSqlSupport",
                        mybatisExportConfig.getMapperPackage() + "." + camelCaseTableName + "DynamicSqlSupport");
            }
        }
        ConfigConstants.namePackageMap.putAll(nameImportMap);

        // 把作者名称放入全局变量
        ConfigConstants.globalParam.put("author", mybatisExportConfig.getAuthor());
        // 放入全局参数
        if (null != mybatisExportConfig.getCustomProperties()) {
            ConfigConstants.globalParam.putAll(mybatisExportConfig.getCustomProperties());
        }
        // 调用 mybatis generator 生成文件
        mapperGeneratorStrategyContext.getMapperGeneratorStrategy(mybatisExportConfig).generator(mybatisExportConfig);

        // 弹框
        DialogFactory.successAndOpenFileDialog(NodeConstants.primaryStage, "导出", "成功", mybatisExportConfig.getProjectDir());

        ConfigConstants.extraTemplateFileConfigs = null;
        ConfigConstants.globalParam.clear();
        ConfigConstants.namePackageMap.clear();
    }

    /**
     * 保存
     */
    public void saveSetup() {
        this.saveSetupInternal();
    }

    private void saveSetupInternal() {
        // 写入文件
        configService.saveConfigToFile();

        // 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 table 配置（如 insert）文件重写
        tableService.downLoadTableIfOverrideModify();

        // 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 columns 文件重写
        columnService.downLoadColumnOverride();
    }
}
