# 初始状态：堆栈中包含了单个元素的元组，即 [(1,), (2,), (3,), (4,), (5,)]。
# 弹出 (1,)，生成 (1, 2), (1, 3), (1, 4), (1, 5) 并添加到堆栈。
# 弹出 (1, 2)，生成 (1, 2, 3), (1, 2, 4), (1, 2, 5) 并添加到堆栈。
# 弹出 (1, 2, 3)，此时长度达到 k = 3，将其添加到结果中。
# 弹出 (1, 2, 4)，此时长度达到 k = 3，将其添加到结果中
# 弹出 (1, 2, 5)，此时长度达到 k = 3，将其添加到结果中。
# 弹出 (1, 3)，生成 (1, 3, 4), (1, 3, 5) 并添加到堆栈。
# 弹出 (1, 3, 4)，此时长度达到 k = 3，将其添加到结果中。
# 弹出 (1, 3, 5)，此时长度达到 k = 3，将其添加到结果中。
# 弹出 (1, 4)，生成 (1, 4, 5) 并添加到堆栈。
# 弹出 (1, 4, 5)，此时长度达到 k = 3，将其添加到结果中。
# 弹出 (1, 5)，无法再生成更长的组合。 **************************
# 弹出 (2,)，生成 (2, 3), (2, 4), (2, 5) 并添加到堆栈。
# 弹出 (2, 3)，生成 (2, 3, 4), (2, 3, 5) 并添加到堆栈。
# 弹出 (2, 3, 4)，此时长度达到 k = 3，将其添加到结果中。
# 弹出 (2, 3, 5)，此时长度达到 k = 3，将其添加到结果中。
# 弹出 (2, 4)，生成 (2, 4, 5) 并添加到堆栈。
# 弹出 (2, 4, 5)，此时长度达到 k = 3，将其添加到结果中。
# 弹出 (2, 5)，无法再生成更长的组合。 ***************************
# 弹出 (3,)，生成 (3, 4), (3, 5) 并添加到堆栈。
# 弹出 (3, 4)，生成 (3, 4, 5) 并添加到堆栈。
# 弹出 (3, 4, 5)，此时长度达到 k = 3，将其添加到结果中。
# 弹出 (3, 5)，无法再生成更长的组合。 ***************************
# 弹出 (4,)，生成 (4, 5) 并添加到堆栈。
# 弹出 (4, 5)，无法再生成更长的组合。
# 弹出 (5,)，无法再生成更长的组合。

# (4,) (5,) is bigger than n - k + 1, so could be eliminated

# index form 1
def combinations(n, k):
    result = []  # 存储生成的组合结果
    stack = [(i,) for i in range(1, n - k + 2)]  # 初始化堆栈，包含单个元素的元组 1 ~ n - k +1

    while stack:  # 循环直到堆栈为空
        current = stack.pop()  # 弹出当前元组
        if len(current) == k:  # 如果元组长度 达到 k
            result.append(current)  # 将元组添加到结果列表中
        else:
            last_element = current[-1] if current else 0  # 获取当前元组的最后一个元素
            for i in range(last_element + 1, n + 1):  # 遍历可能的元素
                stack.append(current + (i,))  # 将新元组推回堆栈

    return result  # 返回生成的组合结果


# It isn't the best implement.
def combinations_helper(arr, k, start, result, current):
    if k == 0:
        result.append(current[:])
        return
    for i in range(start, len(arr)):
        current.append(arr[i])
        combinations_helper(arr, k - 1, i + 1, result, current)
        current.pop()

def combinations_rec(n, k):
    result = []
    combinations_helper(list(range(1, n + 1)), k, 0, result, [])
    return result
