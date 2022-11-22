package com.alan344.service;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.exception.BizException;
import com.alan344.utils.BeanUtils;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONWriter;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2022/11/19 23:35
 */
@Service
public class ExtraFileConfigService {

    private List<ExtraFileConfig> extraFileConfigs;

    public void addExtraFileConfig(ExtraFileConfig extraFileConfig, Integer... index) {
        final List<ExtraFileConfig> extraFileConfigList = this.getExtraFileConfigList();
        if (extraFileConfigList.stream().anyMatch(extraFileConfig1 -> extraFileConfig1.getName().equals(extraFileConfig.getName()))) {
            throw new BizException(extraFileConfig.getName() + "配置名称已经存在");
        }

        if (index != null) {
            extraFileConfigList.add(index[0], extraFileConfig);
        } else {
            extraFileConfigList.add(extraFileConfig);
        }
    }

    public void updateExtraFileConfig(ExtraFileConfig extraFileConfig) {
        final List<ExtraFileConfig> extraFileConfigList = this.getExtraFileConfigList();
        final Optional<ExtraFileConfig> first = extraFileConfigList.stream().filter(extraFileConfig1 -> extraFileConfig1.getName().equals(extraFileConfig.getName())).findFirst();
        if (first.isPresent()) {
            final ExtraFileConfig extraFileConfig1 = first.get();
            if (BeanUtils.checkPropertyOfBean(extraFileConfig1, extraFileConfig)) {
                this.deleteExtraFileConfig(extraFileConfig1);
                extraFileConfigList.add(extraFileConfig);
                this.saveExtraFileConfig();
            }
        }
    }

    public void deleteExtraFileConfig(ExtraFileConfig extraFileConfig) {
        extraFileConfigs.removeIf(extraFileConfig1 -> extraFileConfig1.getName().equals(extraFileConfig.getName()));
    }

    public void saveExtraFileConfig() {
        try {
            FileUtils.writeStringToFile(BaseConstants.getExtraFileConfigFile(), JSONArray.toJSONString(this.getExtraFileConfigList(), JSONWriter.Feature.PrettyFormat, JSONWriter.Feature.WriteEnumsUsingName), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public Map<String, ExtraFileConfig> getExtraFileConfigMap() {
        final List<ExtraFileConfig> extraFileConfigList = this.getExtraFileConfigList();
        return extraFileConfigList.stream().collect(Collectors.toMap(ExtraFileConfig::getName, Function.identity()));
    }

    public List<ExtraFileConfig> getExtraFileConfigList() {
        if (null != extraFileConfigs) {
            return extraFileConfigs;
        }


        final File extraFileConfigFile = BaseConstants.getExtraFileConfigFile();
        if (!extraFileConfigFile.exists()) {
            extraFileConfigs = new ArrayList<>();
        } else {
            try {
                extraFileConfigs = JSONArray.parseArray(FileUtils.readFileToString(extraFileConfigFile, StandardCharsets.UTF_8)).toList(ExtraFileConfig.class);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

        }
        return extraFileConfigs;
    }
}
