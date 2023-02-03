package com.alan344.bean.config;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

/**
 * @author AlanSun
 * @date 2023/1/30 15:58
 */
@Getter
@Setter
public class ExtraTemplateFileGroupConfig implements Cloneable {

    private String groupName;

    private List<ExtraTemplateFileConfig> extraTemplateFileConfigList;

    @Override
    public ExtraTemplateFileGroupConfig clone() {
        try {
            ExtraTemplateFileGroupConfig clone = (ExtraTemplateFileGroupConfig) super.clone();
            // TODO: copy mutable state here, so the clone can't change the internals of the original
            clone.setExtraTemplateFileConfigList(extraTemplateFileConfigList.stream().map(ExtraTemplateFileConfig::clone).toList());
            return clone;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }
}
