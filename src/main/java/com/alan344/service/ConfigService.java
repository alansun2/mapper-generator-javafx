package com.alan344.service;

import com.alan344.bean.GeneratorConfig;
import com.alan344.constants.BaseConstants;
import com.alibaba.fastjson.JSONArray;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author AlanSun
 * @date 2019/8/15 18:10
 */
@Slf4j
@Service
public class ConfigService {
    /**
     * 添加配置
     *
     * @param generatorConfig 配置信息
     */
    public void addConfig(GeneratorConfig generatorConfig) {
        File configFile = BaseConstants.getConfigFile();
        List<GeneratorConfig> generatorConfigs;
        if (configFile.exists()) {
            generatorConfigs = this.loadConfigFromFile();
        } else {
            generatorConfigs = new ArrayList<>();
        }

        generatorConfigs.add(generatorConfig);

        this.downLoadConfigToFile(generatorConfigs);
    }

    /**
     * 从文件加载配置至pane
     */
    public List<GeneratorConfig> loadConfigFromFile() {
        File file = BaseConstants.getConfigFile();
        if (!file.exists()) {
            return Collections.emptyList();
        }

        try {
            return JSONArray.parseArray(FileUtils.readFileToString(file), GeneratorConfig.class);
        } catch (IOException e) {
            log.error("加载dataSource文件失败", e);
            return Collections.emptyList();
        }
    }

    /**
     * 把配置写入文件
     *
     * @param generatorConfigs 配置信息
     */
    private void downLoadConfigToFile(List<GeneratorConfig> generatorConfigs) {
        String configsStr = JSONArray.toJSONString(generatorConfigs);
        try {
            FileUtils.writeStringToFile(BaseConstants.getConfigFile(), configsStr);
        } catch (IOException e) {
            log.error("写入配置信息失败", e);
        }
    }


}
