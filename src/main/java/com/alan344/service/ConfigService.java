package com.alan344.service;

import com.alan344.bean.GeneratorConfig;
import com.alan344.constants.BaseConstants;
import com.alibaba.fastjson.JSONArray;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
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
     * 从文件加载配置至pane
     */
    public List<GeneratorConfig> loadConfigFromFile() {
        File file = new File(BaseConstants.MG_CONFIG_HOME);
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
}
