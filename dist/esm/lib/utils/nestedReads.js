export function addDeletedToSelect(params, config) {
    if (params.args.select && !params.args.select[config.field]) {
        return {
            ...params,
            args: {
                ...params.args,
                select: {
                    ...params.args.select,
                    [config.field]: true,
                },
            },
        };
    }
    return params;
}
export function stripDeletedFieldFromResults(results, config) {
    if (Array.isArray(results)) {
        results === null || results === void 0 ? void 0 : results.forEach((item) => {
            delete item[config.field];
        });
    }
    else if (results) {
        delete results[config.field];
    }
    return results;
}
export function isDeletedFieldOverWritten(field, where) {
    if (!where) {
        return false;
    }
    if (where[field] !== undefined) {
        return true;
    }
    if (where.OR && Array.isArray(where.OR)) {
        const isDeletedFieldOverWrittenInOR = where.OR.some((arg) => {
            return isDeletedFieldOverWritten(field, arg);
        });
        if (isDeletedFieldOverWrittenInOR) {
            return true;
        }
    }
    if (where.AND && Array.isArray(where.AND)) {
        const isDeletedFieldOverWrittenInAND = where.AND.some((arg) => {
            return isDeletedFieldOverWritten(field, arg);
        });
        if (isDeletedFieldOverWrittenInAND) {
            return true;
        }
    }
    return false;
}
