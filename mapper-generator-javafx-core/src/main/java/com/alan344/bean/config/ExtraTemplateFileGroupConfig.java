package com.alan344.bean.config;

import com.alan344.componet.LeftRightLinkageBorderPane;
import lombok.Getter;
import lombok.Setter;

import java.util.Collection;

/**
 * @author AlanSun
 * @date 2023/1/30 15:58
 */
@Getter
@Setter
public class ExtraTemplateFileGroupConfig implements LeftRightLinkageBorderPane.GroupName<ExtraTemplateFileConfig>, Cloneable {
    /**
     * 分组名称
     */
    private String groupName;
    /**
     * 是否是系统内置的配置
     */
    private Boolean isSystem;

    private Collection<ExtraTemplateFileConfig> extraTemplateFileConfigList;

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

    @Override
    public Collection<ExtraTemplateFileConfig> getList() {
        return this.extraTemplateFileConfigList;
    }

    @Override
    public void setList(Collection<ExtraTemplateFileConfig> list) {
        this.extraTemplateFileConfigList = list;
    }
}
